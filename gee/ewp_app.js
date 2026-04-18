var sitePolygons = ee.FeatureCollection("projects/ee-garimayadav/assets/boundary_all_sites"),
    counties = ee.FeatureCollection("TIGER/2018/Counties"),
    maskRaster = ee.Image("projects/ee-garimayadav/assets/EWP_SDI_2002_UTM19_Prs_Abs"),
    statesFeature = ee.FeatureCollection("TIGER/2018/States"),
    counties2 = ee.FeatureCollection("TIGER/2018/Counties");


// ============================================================
//  🌲 Eastern White Pine Health Assessment App
//  Redesigned UI — card-based layer selection, pill year picker
// ============================================================

var counties = counties2;

var stateDict = { 'Maine': '23', 'New Hampshire': '33', 'Vermont': '50' };
var states = Object.keys(stateDict);

// ── Shared state ─────────────────────────────────────────────
var selectedYear = null;
var drawnFeature = null;
var yearButtons = [];
var layerButtons = {};

var LAYERS = [
  { key: 'canopyHeight', label: 'Canopy Height',      sub: 'meters',         asset: 'Canopy_Height_predictions', vis: { min: 0,   max: 40,   palette: ['#d73027','#fee08b','#1a9850'] } },
  { key: 'treeCount',    label: 'Tree Count',          sub: 'per 100 m²',     asset: 'TC_predictions',            vis: { min: 0,   max: 600,  palette: ['#d73027','#fee08b','#1a9850'] } },
  { key: 'lcr',          label: 'Live Crown Ratio',    sub: 'LCR %',          asset: 'LCR_predictions',           vis: { min: 0,   max: 100,  palette: ['#d73027','#fee08b','#1a9850'] } },
  { key: 'lai',          label: 'Leaf Area Index',     sub: 'LAI',            asset: 'LAI_predictions',           vis: { min: 1.5, max: 4.12, palette: ['#d73027','#fee08b','#1a9850'] } },
  { key: 'dtHealth',     label: 'DT Health',           sub: 'Das et al. 2024',asset: 'dt_health_predictions',     vis: { min: 0,   max: 40,   palette: ['#d73027','#fee08b','#1a9850'] } },
  { key: 'clHealth',     label: 'CL Health',           sub: 'classification', asset: 'cl_health_predictions',     vis: { min: 0,   max: 40,   palette: ['#d73027','#fee08b','#1a9850'] } },
];

// Active layer set (keys)
var activeLayers = { treeCount: true };

// ── Colour helpers ────────────────────────────────────────────
var C = {
  darkGreen:  '#1a3a1a',
  midGreen:   '#2a4a2a',
  lightGreen: '#a8d5a2',
  mutedGreen: '#6aaa64',
  activeBg:   '#eaf3de',
  activeBorder:'#4a8a44',
  activeText: '#3b6d11',
  panelBg:    '#ffffff',
  border:     '#e0e0e0',
  textPrimary:'#1a1a1a',
  textMuted:  '#666666',
  warnBg:     '#faeeda',
  warnText:   '#854f0b',
  successBg:  '#eaf3de',
  successText:'#3b6d11',
};

// ── Panel skeleton ────────────────────────────────────────────
var controlPanel = ui.Panel({
  style: {
    width: '300px',
    padding: '0px',
    backgroundColor: C.panelBg,
    border: '1px solid ' + C.border,
  }
});

// ── Header ───────────────────────────────────────────────────
var header = ui.Panel({
  style: { backgroundColor: C.darkGreen, padding: '14px 16px', margin: '0' }
});
header.add(ui.Label({
  value: 'Eastern White Pine',
  style: { fontSize: '20px', fontWeight: 'bold', color: C.lightGreen, backgroundColor: C.darkGreen, textAlign: 'center', margin: '0' }
}));
header.add(ui.Label({
  value: 'Health Assessment App',
  style: { fontSize: '15px', color: C.mutedGreen, backgroundColor: C.darkGreen, textAlign: 'center', margin: '4px 0 0' }
}));

// ── Section helper ────────────────────────────────────────────
function makeSection(content, noBorder) {
  var p = ui.Panel({
    style: {
      padding: '12px 14px',
      backgroundColor: C.panelBg,
      border: noBorder ? '0px' : '0px',
      margin: '0'
    }
  });
  if (content) {
    if (Array.isArray(content)) { content.forEach(function(w){ p.add(w); }); }
    else { p.add(content); }
  }
  return p;
}

function sectionLabel(text) {
  return ui.Label({
    value: text,
    style: { fontSize: '10px', fontWeight: 'bold', color: C.textMuted, margin: '0 0 6px' }
  });
}

function dividerLabel(text) {
  return ui.Label({
    value: '— ' + text + ' —',
    style: { fontSize: '11px', color: C.textMuted, textAlign: 'center', margin: '4px 0', padding: '0 14px' }
  });
}

// ── Status bar ────────────────────────────────────────────────
var statusPanel = ui.Panel({
  style: { padding: '7px 14px', margin: '0', shown: false, backgroundColor: C.warnBg }
});
var statusLabel = ui.Label({ value: '', style: { fontSize: '11px', color: C.warnText } });
statusPanel.add(statusLabel);

function showStatus(text, type) {
  var bg   = type === 'success' ? C.successBg  : type === 'error' ? '#fcebeb' : C.warnBg;
  var col  = type === 'success' ? C.successText : type === 'error' ? '#a32d2d' : C.warnText;
  statusPanel.style().set({ backgroundColor: bg, shown: true });
  statusLabel.style().set('color', col);
  statusLabel.setValue(text);
}

// ── Location section ─────────────────────────────────────────
var stateSelect = ui.Select({
  items: states,
  placeholder: 'Select state…',
  style: { fontSize: '13px', margin: '0 4px 0 0', width: '128px' }
});
var countySelect = ui.Select({
  items: [],
  placeholder: 'Select county…',
  style: { fontSize: '13px', margin: '0', width: '128px' }
});

stateSelect.onChange(function(sel) {
  var code = stateDict[sel];
  var filteredState = statesFeature.filter(ee.Filter.eq('STATEFP', code));
  Map.centerObject(filteredState, 7);
  Map.addLayer(filteredState, { color: 'black' }, sel);
  counties.filter(ee.Filter.eq('STATEFP', code))
    .aggregate_array('NAME').evaluate(function(list) {
      countySelect.items().reset(list);
    });
});

countySelect.onChange(function(sel) {
  var state = stateSelect.getValue();
  if (!state) { showStatus('Select a state first.', 'warn'); return; }
  var code = stateDict[state];
  var feat = counties.filter(ee.Filter.and(
    ee.Filter.eq('STATEFP', code),
    ee.Filter.eq('NAME', sel)
  ));
  Map.centerObject(feat, 9);
  Map.addLayer(feat, { color: '#888888' }, sel);
});

var locationRow = ui.Panel({
  widgets: [stateSelect, countySelect],
  layout: ui.Panel.Layout.Flow('horizontal'),
  style: { margin: '0' }
});

// ── Site select ───────────────────────────────────────────────
var siteSelect = ui.Select({
  placeholder: 'Select a named site…',
  style: { fontSize: '13px', width: '260px' }
});
sitePolygons.aggregate_array('site').distinct().evaluate(function(list) {
  siteSelect.items().reset(list);
});

// ── Draw AOI ──────────────────────────────────────────────────
var drawingTools = Map.drawingTools();
drawingTools.setShown(false);

var drawButton = ui.Button({
  label: '✏  Draw area of interest',
  style: {
    fontSize: '13px',
    color: C.textMuted,
    backgroundColor: C.panelBg,
    border: '1px dashed ' + C.border,
    padding: '7px',
    margin: '6px 0 0',
    width: '260px'
  },
  onClick: function() {
    drawingTools.layers().forEach(function(l) { drawingTools.layers().remove(l); });
    drawnFeature = undefined;
    drawingTools.setShown(true);
    drawingTools.setLinked(false);
    drawingTools.draw();
    showStatus('Draw a polygon on the map.', 'warn');
  }
});

drawingTools.onDraw(function(geometry) {
  if (geometry) {
    drawnFeature = ee.FeatureCollection(geometry);
    Map.addLayer(geometry, { color: 'purple' }, 'Drawn AOI');
    showStatus('AOI drawn successfully.', 'success');
  }
  drawingTools.setShown(false);
});

// ── Year pills ────────────────────────────────────────────────
var years = ['2023', '2024', '2025'];
var yearRow = ui.Panel({ layout: ui.Panel.Layout.Flow('horizontal'), style: { margin: '0' } });

function makeYearButton(yr) {
  var btn = ui.Button({
    label: yr,
    style: {
      fontSize: '12px',
      margin: '0 4px 0 0',
      padding: '5px 0',
      width: '80px',
      textAlign: 'center',
      backgroundColor: C.panelBg,
      border: '1px solid ' + C.border,
      color: C.textMuted
    },
    onClick: function() {
      selectedYear = yr;
      yearButtons.forEach(function(b) {
        var isActive = b.getLabel() === yr;
        b.style().set({
          backgroundColor: isActive ? C.darkGreen : C.panelBg,
          color:           isActive ? C.lightGreen : C.textMuted,
          border: '1px solid ' + (isActive ? C.darkGreen : C.border)
        });
      });
    }
  });
  yearButtons.push(btn);
  yearRow.add(btn);
}
years.forEach(makeYearButton);

// ── Layer cards ───────────────────────────────────────────────
var layersGrid = ui.Panel({ style: { margin: '0' } });

// Two-column layout using paired row panels
for (var i = 0; i < LAYERS.length; i += 2) {
  var row = ui.Panel({ layout: ui.Panel.Layout.Flow('horizontal'), style: { margin: '0 0 6px' } });
  [LAYERS[i], LAYERS[i + 1]].forEach(function(layer) {
    if (!layer) return;
    var isActive = !!activeLayers[layer.key];
    var card = makeLayerCard(layer, isActive);
    layerButtons[layer.key] = card;
    row.add(card);
  });
  layersGrid.add(row);
}

function makeLayerCard(layer, isActive) {
  // ui.Panel has no onClick — use a ui.Button styled as a card.
  // A sub-label sits in a wrapper panel beneath the button.

  var btn = ui.Button({
    label: layer.label,
    style: {
      fontSize: '12px',
      fontWeight: 'bold',
      color: isActive ? C.activeText : C.textMuted,
      backgroundColor: isActive ? C.activeBg : C.panelBg,
      border: '0px',
      width: '124px',
      padding: '6px 10px 2px',
      margin: '0',
      textAlign: 'left'
    }
  });

  var subLabel = ui.Label({
    value: layer.sub,
    style: {
      fontSize: '10px',
      color: isActive ? C.activeText : C.textMuted,
      padding: '0 10px 6px',
      margin: '0'
    }
  });

  var card = ui.Panel({
    widgets: [btn, subLabel],
    style: {
      width: '124px',
      margin: '0 6px 0 0',
      backgroundColor: isActive ? C.activeBg : C.panelBg,
      border: '1px solid ' + (isActive ? C.activeBorder : C.border),
      padding: '0'
    }
  });

  btn.onClick(function() {
    var nowActive = !activeLayers[layer.key];
    activeLayers[layer.key] = nowActive;
    btn.style().set({
      backgroundColor: nowActive ? C.activeBg : C.panelBg,
      color: nowActive ? C.activeText : C.textMuted
    });
    subLabel.style().set('color', nowActive ? C.activeText : C.textMuted);
    card.style().set({
      backgroundColor: nowActive ? C.activeBg : C.panelBg,
      border: '1px solid ' + (nowActive ? C.activeBorder : C.border)
    });
  });

  return card;
}

// ── Apply / Clear buttons ─────────────────────────────────────
var applyButton = ui.Button({
  label: 'Apply layers',
  style: {
    fontSize: '13px',
    fontWeight: 'bold',
    color: C.darkGreen,
    border: '0px',
    padding: '9px 0',
    width: '188px',
    margin: '0'
  },
  onClick: function() {
    if (!selectedYear) { showStatus('Select a year first.', 'error'); return; }
    var anyLayer = Object.keys(activeLayers).some(function(k) { return activeLayers[k]; });
    if (!anyLayer) { showStatus('Select at least one data layer.', 'error'); return; }

    var aoi = drawnFeature || (siteSelect.getValue()
      ? sitePolygons.filter(ee.Filter.eq('site', siteSelect.getValue()))
      : null);

    if (!aoi) { showStatus('Select a site or draw an AOI.', 'error'); return; }

    Map.clear();
    legendPanel.clear();
    legendPanel.style().set('shown', false);

    if (drawnFeature) {
      Map.centerObject(drawnFeature, 10);
    } else {
      Map.centerObject(aoi, 12);
      Map.addLayer(aoi, { color: '#185fa5' }, 'Selected Site');
    }

    loadSelectedLayers(aoi, selectedYear);
  }
});

var clearButton = ui.Button({
  label: 'Clear',
  style: {
    fontSize: '13px',
    color: C.textMuted,
    backgroundColor: C.panelBg,
    padding: '9px 0',
    width: '60px',
    margin: '0 8px 0 0'
  },
  onClick: function() {
    stateSelect.setValue(null, false);
    countySelect.setValue(null, false);
    siteSelect.setValue(null, false);
    selectedYear = null;
    drawnFeature = null;
    activeLayers = { treeCount: true };

    // Reset year buttons
    yearButtons.forEach(function(b) {
      b.style().set({ backgroundColor: C.panelBg, color: C.textMuted, border: '1px solid ' + C.border });
    });

    // Reset layer cards (card = Panel > [Button, Label])
    LAYERS.forEach(function(layer) {
      var card = layerButtons[layer.key];
      if (!card) return;
      var isActive = !!activeLayers[layer.key];
      var btnWidget = card.widgets().get(0);
      var subWidget = card.widgets().get(1);
      card.style().set({
        backgroundColor: isActive ? C.activeBg : C.panelBg,
        border: '1px solid ' + (isActive ? C.activeBorder : C.border)
      });
      btnWidget.style().set({
        backgroundColor: isActive ? C.activeBg : C.panelBg,
        color: isActive ? C.activeText : C.textMuted
      });
      subWidget.style().set('color', isActive ? C.activeText : C.textMuted);
    });

    drawingTools.layers().forEach(function(l) { drawingTools.layers().remove(l); });
    Map.clear();
    legendPanel.clear();
    legendPanel.style().set('shown', false);
    statusPanel.style().set('shown', false);
  }
});

var actionRow = ui.Panel({
  widgets: [clearButton, applyButton],
  layout: ui.Panel.Layout.Flow('horizontal'),
  style: { padding: '12px 14px', margin: '0' }
});

// ── Assemble panel ────────────────────────────────────────────
controlPanel.add(header);
controlPanel.add(makeSection([
  sectionLabel('LOCATION'),
  locationRow
]));
controlPanel.add(dividerLabel('or select a site'));
controlPanel.add(makeSection([siteSelect]));
controlPanel.add(dividerLabel('or draw AOI'));
controlPanel.add(makeSection([drawButton]));
controlPanel.add(makeSection([
  sectionLabel('YEAR'),
  yearRow
]));
controlPanel.add(makeSection([
  sectionLabel('DATA LAYERS'),
  layersGrid
]));
controlPanel.add(statusPanel);
controlPanel.add(actionRow);

ui.root.insert(0, controlPanel);

// ── Raster loading ────────────────────────────────────────────
function loadSelectedLayers(geometryFeature, year) {
  LAYERS.forEach(function(layer) {
    if (!activeLayers[layer.key]) return;
    var assetPath = 'projects/ee-garimayadav/assets/' + layer.asset + '_' + year;
    var raster;
    try {
      raster = ee.Image(assetPath).clip(geometryFeature);
    } catch(e) {
      showStatus('Error loading ' + layer.label + '.', 'error');
      return;
    }
    raster.reduceRegion({
      reducer: ee.Reducer.count(),
      geometry: geometryFeature.geometry(),
      scale: 30,
      maxPixels: 1e13
    }).values().get(0).evaluate(function(val) {
      if (val === null || val === 0 || val === undefined) {
        showStatus('No data for ' + layer.label + '.', 'warn');
      } else {
        Map.addLayer(raster, layer.vis, layer.label);
        addLegend(layer.label, layer.vis.min, layer.vis.max);
        showStatus('Layers loaded for ' + year + '.', 'success');
      }
    });
  });
}

// ── Legend panel ──────────────────────────────────────────────
var legendPanel = ui.Panel({
  style: {
    position: 'bottom-left',
    padding: '10px 12px',
    backgroundColor: 'rgba(255,255,255,0.92)',
    border: '1px solid ' + C.border,
    width: '210px',
    shown: false
  }
});
ui.root.add(legendPanel);

var legendTitle = ui.Label({
  value: 'LEGEND',
  style: { fontSize: '10px', fontWeight: 'bold', color: C.textMuted, margin: '0 0 8px' }
});

function addLegend(layerName, min, max) {
  if (legendPanel.widgets().length === 0) {
    legendPanel.add(legendTitle);
  }
  // Avoid duplicates
  var exists = false;
  legendPanel.widgets().forEach(function(w) {
    if (w.getValue && w.getValue() === layerName) exists = true;
  });
  if (exists) return;

  legendPanel.add(ui.Label({
    value: layerName,
    style: { fontSize: '11px', fontWeight: 'bold', color: C.textPrimary, margin: '6px 0 2px' }
  }));

  var gradient = ui.Thumbnail({
    image: ee.Image.pixelLonLat().select(0).multiply((max - min) / 100).add(min),
    params: {
      bbox: [0, 0, 100, 10],
      dimensions: '180x10',
      format: 'png',
      min: min,
      max: max,
      palette: ['#d73027', '#fee08b', '#1a9850']
    },
    style: { stretch: 'horizontal', margin: '2px 0' }
  });

  var minLbl = ui.Label({ value: String(min), style: { fontSize: '10px', color: C.textMuted } });
  var maxLbl = ui.Label({ value: String(max), style: { fontSize: '10px', color: C.textMuted, textAlign: 'right' } });
  var barRow = ui.Panel({
    widgets: [minLbl, gradient, maxLbl],
    layout: ui.Panel.Layout.Flow('horizontal'),
    style: { margin: '0' }
  });

  legendPanel.add(barRow);
  legendPanel.style().set('shown', true);
}