accessing
methods
	"Return all the methods defined in this package. Including extension methods (i.e., methods defined on a class that is not defined by me)"
	| methods |
	
	methods := OrderedCollection new.
	
	metaclassExtensionSelectors keysAndValuesDo: [:key :val | 
		val do: [:sel | 	methods add: ((Smalltalk globals at: key) theMetaClass >> sel)]].
	classExtensionSelectors keysAndValuesDo: [:key :val |
		val do: [:sel | methods add: ((Smalltalk globals at: key) >> sel)]].
	
	metaclassDefinedSelectors keysAndValuesDo: [:key :val | 
		val do: [:sel | methods add: ((Smalltalk globals at: key) theMetaClass >> sel)]].
	classDefinedSelectors keysAndValuesDo: [:key :val | 
		val do: [:sel | methods add: ((Smalltalk globals at: key) >> sel)]].
	
	^ methods