Extension { #name : #VariableBreakpoint }

{ #category : #'*NewTools-Debugger-Breakpoints-Tools' }
VariableBreakpoint >> asBreakpointInspectionItemIn: aStBreakpointInspectionItem [ 
	^aStBreakpointInspectionItem buildVariableBreakpointItemFor: self
]

{ #category : #'*NewTools-Debugger-Breakpoints-Tools' }
VariableBreakpoint >> printContentsForBreakpointInspection [

	| str |

	str := WriteStream on: String new.
	str << self class name.
	str cr.
	str << '"Break on'.
	str space.
	self isObjectCentric ifTrue: [ 
		str << 'self'.
		str space ].
	str << accessStrategy asString.
	str space.
	str << 'accesses to:"'.

	str cr.

	self targetVariableNames do: [ :varName | 
		str << varName asString.
		str << '.'.
		str space ].

	^ str contents
]

{ #category : #'*NewTools-Debugger-Breakpoints-Tools' }
VariableBreakpoint >> printTargetForBreakpointInspection [

	| str |
	str := WriteStream on: String new.
	str << accessStrategy asString.
	str space.
	str << 'accesses to'.
	str space.
	str << (targetClassOrMethod isCompiledMethod
		 ifTrue: [ 'temp' ]
		 ifFalse: [ 'inst' ]).
	str space.
	str << 'var(s):'.
	str space.
	self targetVariableNames do: [ :name | 
		str nextPutAll: name.
		str space ].
	^ str contents
]

{ #category : #'*NewTools-Debugger-Breakpoints-Tools' }
VariableBreakpoint >> scopeIncludes: anObject [

	| classScope |
	self isObjectCentric ifTrue: [ ^ targetInstance == anObject ].
	classScope := targetClassOrMethod isCompiledMethod
		              ifTrue: [ targetClassOrMethod methodClass ]
		              ifFalse: [ targetClassOrMethod ].
	^ classScope = anObject class
]
