"
I am a trait that makes a class into a singleton whose unique instance is accessed via the method TSingleton>>uniqueInstance.
"
Trait {
	#name : #TSingleton,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #SingletonTrait
}

{ #category : #'instance creation' }
TSingleton classSide >> new [
	self error: 'Use #uniqueInstance'
]

{ #category : #'instance creation' }
TSingleton classSide >> reset [
	<script>
	uniqueInstance := nil.

]

{ #category : #accessing }
TSingleton classSide >> uniqueInstance [
	^ uniqueInstance ifNil: [ uniqueInstance := super new ]
]
