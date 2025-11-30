Extension { #name : 'ToolRegistry' }

{ #category : '*Deprecated14' }
ToolRegistry >> remove: aName [

	self 
		deprecated: 'Use unregisterToolWithRole:' 
		on: '17/11/2024'  
		in: 'pharo13' 
		transformWith:  '`@receiver remove: `@arg'
						-> '`@receiver unregisterToolWithRole: `@arg'.
	self unregisterToolWithRole: aName

]
