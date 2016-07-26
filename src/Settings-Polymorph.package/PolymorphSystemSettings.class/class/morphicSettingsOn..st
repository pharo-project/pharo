morphic
morphicSettingsOn: aBuilder 
	(aBuilder group: #morphic)
		label: 'Morphic';
		description: 'All morphic settings';
		noOrdering;
		with: [
			self morphicWindowsSettingsOn: aBuilder.
			self morphicMenuSettingsOn: aBuilder.
			self morphicHaloSettingsOn: aBuilder.
			(aBuilder setting: #keyboardFocusOnMouseDown)
				label: 'Keyboard focus on mouse down';
				target: MorphicModel;
				description: 'When enabled the mouse button must be clicked within a morph for it to take the keyboard focus.' .
			(aBuilder setting: #mouseOverForKeyboardFocus)
				label: 'Lose keyboard focus when mouse leave';
				target: MorphicModel;
				description: 'When true, the mouse must be over a text or list pane in morphic for keystrokes to be felt, and when the mouse is out over the desktop, the so-called desktop-command-keys, such as cmd-b and cmd-R, are honored.  When false, list panes and text panes in morphic remain sensitive to keystrokes even after the mouse is no longer over the pane.' .
			(aBuilder setting: #balloonHelpEnabled)
				label: 'Balloon Tooltips'; 
				description: 'If true, tooltips are shown when leaving the cursor on top of a graphic element';
				target: UITheme;
				targetSelector: #currentSettings.
			(aBuilder setting: #editableStringMorph)
				label: 'String morphs are editable';
				target: StringMorph;
				description: 'If true, editing on StringMorph is possible if the shift key is pressed while selecting the morphs.' .
			(aBuilder setting: #defaultYellowButtonMenuEnabled)
				label: 'Show default action click menu';
				target: Morph;
				description: 'If true, populate an action click menu (default context menu).'.
			(aBuilder setting: #easySelectingWorld)
				label: 'Easy selection';
				target: WorldState;
				description: 'If true, select multiple Morphs in the world by dragging a selection box around them.']