"
Marker Trait for rules within SUnit group
"
Trait {
	#name : 'ReTSUnitGroupedRule',
	#category : 'SUnit-Rules-Traits',
	#package : 'SUnit-Rules',
	#tag : 'Traits'
}

{ #category : 'accessing' }
ReTSUnitGroupedRule >> group [
	^ ReAbstractRule sunitGroup
]

{ #category : 'accessing' }
ReTSUnitGroupedRule >> groupSelector [

	^ #sunitGroup
]
