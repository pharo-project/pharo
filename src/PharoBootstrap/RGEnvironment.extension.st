Extension { #name : 'RGEnvironment' }

{ #category : '*PharoBootstrap' }
RGEnvironment >> allClasses [

	^ self ask behaviors
]

{ #category : '*PharoBootstrap' }
RGEnvironment >> cleanUnusedUnreferencedPackages [

	"remove packages that are not not used"

	| referenced unreferenced |

	referenced := IdentitySet new.
	self ask behaviorsDo: [ :each |
		referenced addAll: each referencedPackages ].

	unreferenced := (self ask packages copyWithoutAll: referenced).
	"Do not skip packages that only have extension methods..."
	unreferenced := unreferenced reject: [ :e | e definedBehaviors isEmpty and: [ e extensionMethods notEmpty ] ].

	unreferenced do: [:each | self removePackage: each  ].

	^ unreferenced
]

{ #category : '*PharoBootstrap' }
RGEnvironment >> includesClassNamed: aName [ 

	^ self ask includesClassNamed: aName 
]
