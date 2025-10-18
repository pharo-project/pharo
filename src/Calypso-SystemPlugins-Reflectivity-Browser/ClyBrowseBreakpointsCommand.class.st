"
I am a command to open browser with methods where halts or breakpoints are installed.

I am registered in world toolbar using class side method #worldMenuOn:
"
Class {
	#name : 'ClyBrowseBreakpointsCommand',
	#superclass : 'CmdCommand',
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-Breakpoints',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'Breakpoints'
}

{ #category : 'accessing' }
ClyBrowseBreakpointsCommand >> defaultMenuIconName [

	^#smallDebug
]

{ #category : 'accessing' }
ClyBrowseBreakpointsCommand >> defaultMenuItemName [

	^'Browse Breakpoints'
]

{ #category : 'accessing' }
ClyBrowseBreakpointsCommand >> description [

	^'Open browser with methods where Halts or Breakpoints are installed'
]

{ #category : 'execution' }
ClyBrowseBreakpointsCommand >> execute [

	ClyQueryBrowserMorph browseBreakpoints
]
