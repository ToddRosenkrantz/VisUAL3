/*
Visual2 @ Imperial College London
Project: A user - friendly ARM emulator in F# and Web Technologies(Github Electron & Fable Compliler)
Module: JS/Monaco-init
Description: Javascript code to run Monaco editor and define code highlighting regexes
*/


// Save Monaco's amd require and restore Node's require
var amdRequire = global.require;
global.require = nodeRequire;

var path = require('path');

function uriFromPath(_path) {
  var pathName = path.resolve(_path).replace(/\\/g, '/');
  if (pathName.length > 0 && pathName.charAt(0) !== '/') {
    pathName = '/' + pathName;
  }
  return encodeURI('file://' + pathName);
}
amdRequire.config({
  baseUrl: uriFromPath(path.join(__dirname, 'js')),
  // Load Monaco's modules with <script> tags, as in a browser. Otherwise, because the renderer
  // has node integration, the loader uses node's vm module, which Electron warns is unsupported.
  preferScriptTags: true
});
amdRequire(['vs/editor/editor.main'], function () {

  monaco.languages.register({
    id: 'arm'
  });
  // ARM mnemonics accepted by the emulator: root + optional suffix + optional condition,
  // the same expansion as the emulator's opcode tables (CommonLex.opCodeExpand)
  function armMnemonics() {
    var conds = ['', 'EQ', 'NE', 'CS', 'HS', 'CC', 'LO', 'MI', 'PL', 'VS', 'VC', 'HI', 'LS', 'GE', 'LT', 'GT', 'LE', 'AL', 'NV'];
    var specs = [
      [['MOV', 'MVN', 'ADD', 'SUB', 'ADC', 'SBC', 'RSB', 'RSC', 'AND', 'ORR', 'EOR', 'BIC',
        'LSL', 'LSR', 'ASR', 'ROR', 'RRX', 'MUL', 'MLA', 'CMP', 'CMN', 'TST', 'TEQ'], ['', 'S']],
      [['LDR', 'STR'], ['', 'B']],
      [['LDM', 'STM'], ['', 'IA', 'IB', 'DA', 'DB', 'FD', 'ED', 'FA', 'EA']],
      [['PUSH', 'POP', 'B', 'BL', 'BX', 'ADR'], ['']]
    ];
    var words = [];
    specs.forEach(function (spec) {
      spec[0].forEach(function (root) {
        spec[1].forEach(function (suffix) {
          conds.forEach(function (cond) { words.push(root + suffix + cond); });
        });
      });
    });
    return words;
  }

  // GNU as (ARM, unified syntax)
  monaco.languages.setMonarchTokensProvider('arm', {
    defaultToken: '',
    ignoreCase: true,
    keywords: armMnemonics(),
    registers: ['r0', 'r1', 'r2', 'r3', 'r4', 'r5', 'r6', 'r7', 'r8', 'r9', 'r10', 'r11', 'r12', 'r13', 'r14', 'r15',
                'sp', 'lr', 'pc', 'fp', 'ip', 'sl', 'sb', 'a1', 'a2', 'a3', 'a4',
                'v1', 'v2', 'v3', 'v4', 'v5', 'v6', 'v7', 'v8'],
    shifts: ['lsl', 'lsr', 'asr', 'ror', 'rrx', 'asl'],
    brackets: [
      ['{', '}', 'delimiter.curly'],
      ['[', ']', 'delimiter.square'],
      ['(', ')', 'delimiter.parenthesis']
    ],
    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]+|[0-7]{1,3})/,

    tokenizer: {
      root: [
        // testbench result lines
        [/^\s*>>;.*$/, 'comment.testpass'],
        [/^\s*>>-.*$/, 'comment.testerror'],
        // '#' as the first character of a line is a comment
        [/^\s*#.*$/, 'comment'],
        // comments
        [/@.*$/, 'comment'],
        [/\/\/.*$/, 'comment'],
        [/\/\*/, 'comment', '@blockComment'],
        // label definitions (name: or numeric local label 1:)
        [/[a-zA-Z_.$][\w.$]*\s*:/, 'type.identifier'],
        [/\d+\s*:/, 'type.identifier'],
        // directives
        [/\.[a-zA-Z_][\w]*/, 'keyword.directive'],
        // mnemonics, registers, shifts, symbols
        [/[a-zA-Z_$][\w.$]*/, {
          cases: {
            '@keywords': 'keyword',
            '@registers': 'variable.predefined',
            '@shifts': 'keyword.shift',
            '@default': 'identifier'
          }
        }],
        // immediates and numbers
        [/#\s*-?0[xX][0-9a-fA-F]+/, 'number.hash.hex'],
        [/#\s*-?0[bB][01]+/, 'number.hash.bin'],
        [/#\s*-?\d+/, 'number.hash'],
        [/#/, 'number.hash'],
        [/0[xX][0-9a-fA-F]+/, 'number.bare.hex'],
        [/0[bB][01]+/, 'number.bare.bin'],
        [/\d+[bf]\b/, 'type.identifier'], // local label reference 1b / 1f
        [/\d+/, 'number.bare'],
        // strings and characters
        [/"([^"\\]|\\.)*$/, 'string.invalid'],
        [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],
        [/'(@escapes|[^\\])'?/, 'string'],
        // delimiters and operators
        [/[{}()\[\]]/, '@brackets'],
        [/[,!]/, 'delimiter'],
        [/<<|>>|[=~+\-*\/%&|^]/, 'operator'],
        [/[ \t\r\n]+/, 'white']
      ],
      blockComment: [
        [/[^\/*]+/, 'comment'],
        [/\*\//, 'comment', '@pop'],
        [/[\/*]/, 'comment']
      ],
      string: [
        [/[^\\"]+/, 'string'],
        [/@escapes/, 'string.escape'],
        [/\\./, 'string.escape.invalid'],
        [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]
      ]
    }
  });

  // Convert CSS-stle hex color (with #) to form needed by syntax highlighting.
  // Add a JS color display extension to have colors displayed in source
  function cs (color)  { 
    return color.substr(1);
  }

  var base03 = '#002b36';
  var base02 = '#073642';
  var base01 = '#586e75';
  var base00 = '#657b83';
  var base0 = '#839496';
  var base1 = '#93a1a1';
  var base2 = '#eee8d5';
  var base3 = '#fdf6e3';
  var yellow = '#b58900';
  var orange = '#cb4b16';
  var red = '#dc322f';
  var magenta = '#d33682';
  var violet = '#6c71c4';
  var blue = '#268bd2';
  var cyan = '#2aa198';
  var green = '#859900';

  monaco.editor.defineTheme('one-light-pro', {
    base: 'vs',
    inherit: true, // can also be false to completely replace the builtin rules
    rules: [
      { token: 'operator', foreground: cs('#303030')},
      { token: 'keyword', foreground: cs('#1010a0')},
      { token: 'symbols', foreground: cs('#303030')},
      { token: 'comment', foreground: cs('#308030')},
      { token: 'comment.testerror', foreground: cs(red) },
      { token: 'escape', foreground: cs('#ff0000')},
      { token: 'string', foreground: cs('#e06c75') },
      {token: 'number.bare', foreground: cs("#c08000") },
      { token: 'number.hash', foreground: cs("#408080")}
    ],
    "colors": {
      'editor.foreground': '#000000',
      'editor.background': '#EDF9FA',
      'editorCursor.foreground': '#8B0000',
      'editor.lineHighlightBackground': base2,
      'editorLineNumber.foreground': base1,
      'editor.selectionBackground': '#CC0080',
      'editor.inactiveSelectionBackground': base3
    }
  });

  monaco.editor.defineTheme('one-dark-pro', {
    base: 'vs-dark',
    inherit: true, // can also be false to completely replace the builtin rules
    rules: [
      { token: 'operator', foreground: cs('#b0b0b0')},
      { token: 'keyword', foreground: cs(blue)},
      { token: 'symbol', foreground: cs('#a0a0a0')},
      { token: 'comment', foreground: cs('#20a020')},
      { token: 'comment.testerror', foreground: cs(red) },
      { token: 'escape', foreground: cs('#57b6c2')},
      { token: 'string', foreground: cs('#e06c75')},
      { token: 'number.hash', foreground: cs("#80c0c0")},
      {token: 'number.bare', foreground: cs("#f0f080")}
    ],
    "colors": {
      'editor.foreground': '#FFFFFF',
      //'editor.background': '#000000',
      'editorCursor.foreground': '#8B0000',
      'editor.lineHighlightBackground': base02,
      'editorLineNumber.foreground': base01,
      'editor.selectionBackground': '#CC0080',
      'editor.inactiveSelectionBackground': base01,
      'editor.findMatchBackground': base00, // Color of the current search match.
      'editor.findMatchHighlightBackground':base02 // Color of the other search matches.
    }
  });

  
  monaco.editor.defineTheme('solarised-light', {
    base: 'vs',
    inherit: true, // can also be false to completely replace the builtin rules
    rules: [
      { token: 'delimiter', foreground: cs(base00)},
      { token: 'identifier', foreground: cs(base00)},
      { token: 'keyword', foreground: cs(blue)},
      { token: 'symbol', foreground: cs(base00)},
      { token: 'comment', foreground: cs(green)},
      { token: 'comment.testerror', foreground: cs(red) },
      { token: 'escape', foreground: cs('#57b6c2')},
      { token: 'string', foreground: cs('#e06c75')},
      { token: 'number.hash', foreground: cs(cyan)},
      {token: 'number.bare', foreground: cs(yellow)}
    ],
    "colors": {
      'foreground': base00,
      'editor.foreground': base00,
      'editor.background': base3,
      'editorCursor.foreground': magenta,
      'editor.lineHighlightBackground': base2,
      'editorLineNumber.foreground': base1,
      'editor.selectionBackground': base1,
      'editor.inactiveSelectionBackground': base1,
      'editor.findMatchBackground': base0, // Color of the current search match.
      'editor.findMatchHighlightBackground':base2 // Color of the other search matches.
    }
  });


    monaco.editor.defineTheme('solarised-dark', {
      base: 'vs-dark',
      inherit: true, // can also be false to completely replace the builtin rules
      rules: [
        { token: 'delimiter', foreground: cs(base0)},
        { token: 'identifier', foreground: cs(base0)},
        { token: 'keyword', foreground: cs(blue)},
        { token: 'symbol', foreground: cs(base0)},
        { token: 'comment', foreground: cs(green)},
        { token: 'comment.testerror', foreground: cs(red) },
        { token: 'escape', foreground: cs('#57b6c2')},
        { token: 'string', foreground: cs('#e06c75')},
        { token: 'number.hash', foreground: cs(cyan)},
        {token: 'number.bare', foreground: cs(yellow)}
      ],
      "colors": {
        'foreground': base0,
        'editor.foreground': base0,
        'editor.background': base03,
        'editorCursor.foreground': magenta,
        'editor.lineHighlightBackground': base02,
        'editorLineNumber.foreground': base01,
        'editor.selectionBackground': base01,
        'editor.inactiveSelectionBackground': base01,
        'editor.findMatchBackground': base00, // Color of the current search match.
        'editor.findMatchHighlightBackground':base02 // Color of the other search matches.
      }
    });
  

    //'foreground' // Overall foreground color. This color is only used if not overridden by a component.
//'errorForeground' // Overall foreground color for error messages. This color is only used if not overridden by a component.
//'descriptionForeground' // Foreground color for description text providing additional information, for example for a label.
//'focusBorder' // Overall border color for focused elements. This color is only used if not overridden by a component.
//'contrastBorder' // An extra border around elements to separate them from others for greater contrast.
//'contrastActiveBorder' // An extra border around active elements to separate them from others for greater contrast.
//'selection.background' // The background color of text selections in the workbench (e.g. for input fields or text areas). Note that this does not apply to selections within the editor.
//'textSeparator.foreground' // Color for text separators.
//'textLink.foreground' // Foreground color for links in text.
//'textLink.activeForeground' // Foreground color for active links in text.
//'textPreformat.foreground' // Foreground color for preformatted text segments.
//'textBlockQuote.background' // Background color for block quotes in text.
//'textBlockQuote.border' // Border color for block quotes in text.
//'textCodeBlock.background' // Background color for code blocks in text.
//'widget.shadow' // Shadow color of widgets such as find/replace inside the editor.
//'input.background' // Input box background.
//'input.foreground' // Input box foreground.
//'input.border' // Input box border.
//'inputOption.activeBorder' // Border color of activated options in input fields.
//'input.placeholderForeground' // Input box foreground color for placeholder text.
//'inputValidation.infoBackground' // Input validation background color for information severity.
//'inputValidation.infoBorder' // Input validation border color for information severity.
//'inputValidation.warningBackground' // Input validation background color for information warning.
//'inputValidation.warningBorder' // Input validation border color for warning severity.
//'inputValidation.errorBackground' // Input validation background color for error severity.
//'inputValidation.errorBorder' // Input validation border color for error severity.
//'dropdown.background' // Dropdown background.
//'dropdown.foreground' // Dropdown foreground.
//'dropdown.border' // Dropdown border.
//'list.focusBackground' // List/Tree background color for the focused item when the list/tree is active. An active list/tree has keyboard focus, an inactive does not.
//'list.focusForeground' // List/Tree foreground color for the focused item when the list/tree is active. An active list/tree has keyboard focus, an inactive does not.
//'list.activeSelectionBackground' // List/Tree background color for the selected item when the list/tree is active. An active list/tree has keyboard focus, an inactive does not.
//'list.activeSelectionForeground' // List/Tree foreground color for the selected item when the list/tree is active. An active list/tree has keyboard focus, an inactive does not.
//'list.inactiveSelectionBackground' // List/Tree background color for the selected item when the list/tree is inactive. An active list/tree has keyboard focus, an inactive does not.
//'list.inactiveSelectionForeground' // List/Tree foreground color for the selected item when the list/tree is inactive. An active list/tree has keyboard focus, an inactive does not.
//'list.hoverBackground' // List/Tree background when hovering over items using the mouse.
//'list.hoverForeground' // List/Tree foreground when hovering over items using the mouse.
//'list.dropBackground' // List/Tree drag and drop background when moving items around using the mouse.
//'list.highlightForeground' // List/Tree foreground color of the match highlights when searching inside the list/tree.
//'pickerGroup.foreground' // Quick picker color for grouping labels.
//'pickerGroup.border' // Quick picker color for grouping borders.
//'button.foreground' // Button foreground color.
//'button.background' // Button background color.
//'button.hoverBackground' // Button background color when hovering.
//'badge.background' // Badge background color. Badges are small information labels, e.g. for search results count.
//'badge.foreground' // Badge foreground color. Badges are small information labels, e.g. for search results count.
//'scrollbar.shadow' // Scrollbar shadow to indicate that the view is scrolled.
//'scrollbarSlider.background' // Slider background color.
//'scrollbarSlider.hoverBackground' // Slider background color when hovering.
//'scrollbarSlider.activeBackground' // Slider background color when active.
//'progressBar.background' // Background color of the progress bar that can show for long running operations.
//'editor.background' // Editor background color.
//'editor.foreground' // Editor default foreground color.
//'editorWidget.background' // Background color of editor widgets, such as find/replace.
//'editorWidget.border' // Border color of editor widgets. The color is only used if the widget chooses to have a border and if the color is not overridden by a widget.
//'editor.selectionBackground' // Color of the editor selection.
//'editor.selectionForeground' // Color of the selected text for high contrast.
//'editor.inactiveSelectionBackground' // Color of the selection in an inactive editor.
//'editor.selectionHighlightBackground' // Color for regions with the same content as the selection.
//'editor.findRangeHighlightBackground' // Color the range limiting the search.
//'editor.hoverHighlightBackground' // Highlight below the word for which a hover is shown.
//'editorHoverWidget.background' // Background color of the editor hover.
//'editorHoverWidget.border' // Border color of the editor hover.
//'editorLink.activeForeground' // Color of active links.
//'diffEditor.insertedTextBackground' // Background color for text that got inserted.
//'diffEditor.removedTextBackground' // Background color for text that got removed.
//'diffEditor.insertedTextBorder' // Outline color for the text that got inserted.
//'diffEditor.removedTextBorder' // Outline color for text that got removed.
//'merge.currentHeaderBackground' // Current header background in inline merge-conflicts.
//'merge.currentContentBackground' // Current content background in inline merge-conflicts.
//'merge.incomingHeaderBackground' // Incoming header background in inline merge-conflicts.
//'merge.incomingContentBackground' // Incoming content background in inline merge-conflicts.
//'merge.commonHeaderBackground' // Common ancestor header background in inline merge-conflicts.
//'merge.commonContentBackground' // Common ancester content background in inline merge-conflicts.
//'merge.border' // Border color on headers and the splitter in inline merge-conflicts.
//'editorOverviewRuler.currentContentForeground' // Current overview ruler foreground for inline merge-conflicts.
//'editorOverviewRuler.incomingContentForeground' // Incoming overview ruler foreground for inline merge-conflicts.
//'editorOverviewRuler.commonContentForeground' // Common ancestor overview ruler foreground for inline merge-conflicts.
//'editor.lineHighlightBackground' // Background color for the highlight of line at the cursor position.
//'editor.lineHighlightBorder' // Background color for the border around the line at the cursor position.
//'editor.rangeHighlightBackground' // Background color of highlighted ranges, like by quick open and find features.
//'editorCursor.foreground' // Color of the editor cursor.
//'editorWhitespace.foreground' // Color of whitespace characters in the editor.
//'editorIndentGuide.background' // Color of the editor indentation guides.
//'editorLineNumber.foreground' // Color of editor line numbers.
//'editorRuler.foreground' // Color of the editor rulers.
//'editorCodeLens.foreground' // Foreground color of editor code lenses
//'editorBracketMatch.background' // Background color behind matching brackets
//'editorBracketMatch.border' // Color for matching brackets boxes
//'editorOverviewRuler.border' // Color of the overview ruler border.
//'editorGutter.background' // Background color of the editor gutter. The gutter contains the glyph margins and the line numbers.
//'editorError.foreground' // Foreground color of error squigglies in the editor.
//'editorError.border' // Border color of error squigglies in the editor.



  var mevent = new CustomEvent("monaco-ready", { "detail": "ready now!" });

  // Dispatch/Trigger/Fire the event
  document.dispatchEvent(mevent);
});





