updating
categoryModified: anEvent
	"(anEvent itemClass name asString beginsWith: 'ConfigurationOf')
		ifTrue: [  self respondToEventFor: anEvent itemClass withOperation: #modified ]"
	| configName configClass configInfo |
	self flag: 'update only needed configurations'.
	"configName := 'ConfigurationOf' , (anEvent item).
	configClass := Smalltalk at: (configName asSymbol).
	configClass ifNotNilDo: [ 
		configInfo := self configurationInfoFor: configClass.
		configInfo fullRecalculate ]."
	self configurations do: [:config | config fullRecalculate ].