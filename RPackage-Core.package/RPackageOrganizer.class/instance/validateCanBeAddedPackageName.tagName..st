private
validateCanBeAddedPackageName: packageName tagName: tagName 
	| package |
	
	package := self 
		packageNamed: packageName
		ifAbsent: [ 
			(packageName includes: $-) ifFalse: [ ^ self ].
			 
			^ self 
				validateCanBeAddedPackageName: (packageName copyUpToLast: $-)
				tagName: ((packageName copyAfterLast: $-), (tagName 
					ifNotNil: [ '-', tagName ]
					ifNil: [ '' ] )) ].
		
	(tagName isEmptyOrNil or: [ package includesClassTagNamed: tagName ])
		ifTrue: [ 
			RPackageConflictError signal: ('Package/Tag can not be added because it conflicts with package {1} tag {2}' format: { packageName. tagName })  ]
