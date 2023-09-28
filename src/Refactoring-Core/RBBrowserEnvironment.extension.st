Extension { #name : 'RBBrowserEnvironment' }

{ #category : '*Refactoring-Core' }
RBBrowserEnvironment >> classesInPackages: aColl do: aBlock [

	self systemDictionaryClassesDo: [ :aClass |
		((self includesClass: aClass) and: [
			 | classPackageName |
			 classPackageName := aClass category.
			 aColl anySatisfy: [ :packageName | packageName , '*' match: classPackageName ] ]) ifTrue: [ aBlock value: aClass ] ]
]
