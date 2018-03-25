"
I register settings only needed for GTDebugger.
"
Class {
	#name : #GTDebuggerSystemSettings,
	#superclass : #Object,
	#category : #'GT-Debugger-Model'
}

{ #category : #settings }
GTDebuggerSystemSettings class >> debugSettingsOn: aBuilder [
	<systemsettings>
	
	(aBuilder setting: #enableStackColoring)
		label: 'Color the stack';
		target: GTGenericStackDebugger;
		parent: #debugging;
		description:
				'When true, the stack context are colored based on their relationship with the selected context.'.
	
	(aBuilder setting: #enableDebuggerWindowDistinctColor)
		label: 'Color the window';
		target: GTGenericStackDebugger;
		parent: #debugging;
		description:
				'When true, the window border is colored with red to make it distinct from other windows.'.
	

]
