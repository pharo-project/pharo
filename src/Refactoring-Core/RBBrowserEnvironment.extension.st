Extension { #name : 'RBBrowserEnvironment' }

{ #category : '*Refactoring-Core' }
RBBrowserEnvironment >> classesInPackages: aColl do: aBlock [

	self systemDictionaryClassesDo: [ :aClass |
		((self includesClass: aClass) and: [
			 | classPackageName |
			 classPackageName := aClass package name.
			 aColl anySatisfy: [ :packageName | packageName sameAs: classPackageName ] ]) ifTrue: [ aBlock value: aClass ] ]
]
