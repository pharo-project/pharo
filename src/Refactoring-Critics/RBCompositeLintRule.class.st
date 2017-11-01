"
A RBCompositeLintRule is a composite rule holding rules.
"
Class {
	#name : #RBCompositeLintRule,
	#superclass : #RBLintRule,
	#instVars : [
		'rules',
		'name'
	],
	#category : 'Refactoring-Critics'
}

{ #category : #accessing }
RBCompositeLintRule class >> allRules [
	^ self rules: (Array with: self lintChecks with: self transformations) name: 'All checks'
]

{ #category : #accessing }
RBCompositeLintRule class >> lintChecks [
	^ self rules: (self rulesGroupedFor: RBBasicLintRule) name: 'Lint checks'
]

{ #category : #'instance creation' }
RBCompositeLintRule class >> rules: aCollection [
	^ self new
		rules: aCollection;
		yourself
]

{ #category : #'instance creation' }
RBCompositeLintRule class >> rules: aCollection name: aString [
	^ self new
		rules: aCollection;
		name: aString;
		yourself
]

{ #category : #'instance creation' }
RBCompositeLintRule class >> rulesFor: aRuleClass [
	| rules |
	rules := SortedCollection
		sortBlock: [ :a :b | a name <= b name ].
	aRuleClass withAllSubclassesDo: [ :each |
		each isVisible
			ifTrue: [ rules add: each new ] ].
	^ rules asArray
]

{ #category : #'instance creation' }
RBCompositeLintRule class >> rulesGroupedFor: aRuleClass [
	| groups rules |
	groups := Dictionary new.
	(self rulesFor: aRuleClass) do: [ :each |
		(groups at: each group ifAbsentPut: [ OrderedCollection new ])
			addLast: each ].
	rules := SortedCollection sortBlock: [ :a :b | a name <= b name ].
	groups keysAndValuesDo: [ :group :elements |
		rules addLast: (RBCompositeLintRule 
			rules: elements asArray
			name: group) ].
	^ rules asArray
]

{ #category : #accessing }
RBCompositeLintRule class >> transformations [
	^ self rules: (self rulesGroupedFor: RBTransformationRule) name: 'Transformations'
]

{ #category : #accessing }
RBCompositeLintRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'CompositeLintRule'
]

{ #category : #running }
RBCompositeLintRule >> basicCheck: anEntity [
	"this has to be rewritten in a smarter way, when the complete implementation with critics will be ready".
	
	^ (self leaves select: [ :rule | rule class perform: (
		anEntity isCompiledMethod
			ifTrue: [ #checksMethod ]
			ifFalse: [ #checksClass ]) ]) anySatisfy: [ :rule |
				rule basicCheck: anEntity ]
]

{ #category : #accessing }
RBCompositeLintRule >> changes [
	^ rules flatCollect: [ :each  | each changes ]
]

{ #category : #running }
RBCompositeLintRule >> checkClass: aClass [ 
	rules do: [ :each | each checkClass: aClass ]
]

{ #category : #running }
RBCompositeLintRule >> checkMethod: aMethod [ 
	rules do: [ :each | each checkMethod: aMethod ]
]

{ #category : #running }
RBCompositeLintRule >> checkPackage: aPackage [ 
	rules do: [ :each | each checkPackage: aPackage ]
]

{ #category : #testing }
RBCompositeLintRule >> hasConflicts [
	^ rules anySatisfy: [ :each | each hasConflicts ]
]

{ #category : #testing }
RBCompositeLintRule >> isComposite [
	^ true
]

{ #category : #testing }
RBCompositeLintRule >> isEmpty [
	^ rules allSatisfy: [ :each | each isEmpty ]
]

{ #category : #accessing }
RBCompositeLintRule >> leaves [

	^ rules flatCollect: [ :rule | rule leaves ]	

]

{ #category : #accessing }
RBCompositeLintRule >> name [
	^ name
]

{ #category : #accessing }
RBCompositeLintRule >> name: aString [
	name := aString
]

{ #category : #accessing }
RBCompositeLintRule >> problemCount [
	^ rules inject: 0 into: [ :count :each | count + each problemCount ]
]

{ #category : #adding/removing }
RBCompositeLintRule >> removeRule: aRule [

	(rules anySatisfy: [ :rule | rule name = aRule name] )
		ifTrue: [rules := rules reject:  [ :rule | rule name = aRule name]]
		 ifFalse: [rules do: [ :rule |
				rule isComposite 
					ifTrue: [rule removeRule: aRule]]] 
]

{ #category : #initialization }
RBCompositeLintRule >> resetResult [
	rules do: [ :each | each resetResult ]
]

{ #category : #accessing }
RBCompositeLintRule >> rules [
	^ rules
]

{ #category : #accessing }
RBCompositeLintRule >> rules: aCollection [
	rules := aCollection
]

{ #category : #adding/removing }
RBCompositeLintRule >> sort: aBlock [
	
	rules first isComposite
		ifTrue: [ rules do: [ :rule | rule sort: aBlock ]] 
		ifFalse: [ self rules: (rules sort: aBlock) ]
]
