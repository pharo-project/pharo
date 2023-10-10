"
I am a group of breakpoints.
By default there are two groups called 'All Breakpoints' and 'No Group'. These groups should not be removed as every new breakpoint will be added to them.

To change the name of a group:
aGroup name: aString

To add a breakpoint to a group: 
aGroup addBreakpoint: aBreakpoint

To remove a breakpoint from a group:
aGroup removeBreakpoint: aBreakpoint
"
Class {
	#name : 'BreakpointGroup',
	#superclass : 'Object',
	#instVars : [
		'name',
		'breakpoints'
	],
	#classVars : [
		'AllBreakpointsGroup',
		'AllGroups',
		'UngroupedBreakpointsGroup'
	],
	#category : 'Reflectivity-Breakpoints',
	#package : 'Reflectivity',
	#tag : 'Breakpoints'
}

{ #category : 'accessing' }
BreakpointGroup class >> all [

	^ AllGroups ifNil: [ AllGroups := OrderedCollection new ]
]

{ #category : 'accessing' }
BreakpointGroup class >> allBreakpointsGroup [
	^ AllBreakpointsGroup
]

{ #category : 'development support' }
BreakpointGroup class >> cleanUpAll [
	"resets groups and breakpoints completly (mostly for developing purposes or if something goes wrong)"
	AllGroups do: [ :group | group breakpoints do: [ :bp | group removeBreakpoint: bp ] ].
	Breakpoint copy all do: [ :bp | bp remove ].
	AllGroups copy do: [ :group | group remove ].
	self initialize
]

{ #category : 'class initialization' }
BreakpointGroup class >> initialize [

	AllGroups := OrderedCollection new.
	AllBreakpointsGroup := self new name: 'All Breakpoints'.
	UngroupedBreakpointsGroup := self new name: 'No Group'
]

{ #category : 'instance creation' }
BreakpointGroup class >> newBreakpoint: aBreakpoint [
	AllBreakpointsGroup addBreakpoint: aBreakpoint.
	UngroupedBreakpointsGroup addBreakpoint: aBreakpoint
]

{ #category : 'accessing' }
BreakpointGroup class >> ungroupedBreakpointsGroup [
	^ UngroupedBreakpointsGroup
]

{ #category : 'API' }
BreakpointGroup >> addBreakpoint: aBreakpoint [
	self breakpoints add: aBreakpoint
]

{ #category : 'accessing' }
BreakpointGroup >> breakpoints [
	^breakpoints ifNil: [ breakpoints := WeakSet new]
]

{ #category : 'initialization' }
BreakpointGroup >> initialize [
	AllGroups add: self.
	^self
]

{ #category : 'accessing' }
BreakpointGroup >> name [
	^name ifNil: [ name := 'unnamed group' ]
]

{ #category : 'API' }
BreakpointGroup >> name: aString [
	name := aString
]

{ #category : 'API' }
BreakpointGroup >> remove [
	breakpoints copy do: [ :bp | bp addToGroup: self class ungroupedBreakpointsGroup ].
	AllGroups remove: self
]

{ #category : 'API' }
BreakpointGroup >> removeBreakpoint: aBreakpoint [
	self breakpoints remove: aBreakpoint ifAbsent: []
]
