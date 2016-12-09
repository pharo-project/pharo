running
installMethodsInBehaviorMapping: aBehaviorMapping using: aMethodBuilder onMirror: mirror prefixed: aPrefix
	| newMethods theClass |
	newMethods := (aMethodBuilder methodsFromDefinition: aBehaviorMapping) sorted: [ :a :b | a selector < b selector ].
	theClass := (self classNamed: aBehaviorMapping theNonMetaClass name) mirror.
	aBehaviorMapping isMeta ifTrue: [ theClass := theClass classSide ].
	newMethods doWithIndex: [ :method :index |
		self log: [aPrefix, ' - ', index asString, '/', newMethods size asString, ': Installing method ', 
			aBehaviorMapping name, '>>', method selector ].
		
		self installMethod: method inBehavior: theClass.
	].