Class {
	#name : 'FLEphemeronTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
FLEphemeronTest >> testWeakKeyAssociationMourningAfterMaterialization [
	"This tests serialization / materialization of WeakKeyAssociation, which is an ephemeron class.
	When the key is not strongly referenced, which is the case here, the garbage collector
	will send #mourn to the object to finalize it. This will cause the ephemeron to remove itself
	from its container."

	| dictionary ephemeron materializedDictionary materializedAssociation |
	dictionary := Dictionary new.
	ephemeron := WeakKeyAssociation new
		key: Object new;
		value: 'value';
		container: dictionary.
	dictionary add: ephemeron.

	materializedDictionary := self resultOfSerializeAndMaterialize: dictionary.
	self deny: materializedDictionary isEmpty.
	self assert: materializedDictionary size equals: 1.
	materializedAssociation := materializedDictionary associations first.
	self assert: materializedAssociation key isNil.
	self assert: materializedAssociation value equals: 'value'.
		
	Smalltalk garbageCollect.
	self deny: materializedDictionary isEmpty
]

{ #category : 'tests' }
FLEphemeronTest >> testWeakKeyAssociationMourningBeforeMaterialization [
	"This tests serialization / materialization of WeakKeyAssociation, which is an ephemeron class.
	When the key is not strongly referenced, which is the case here, the garbage collector
	will send #mourn to the object to finalize it. This will cause the ephemeron to remove itself
	from its container."

	| dictionary ephemeron materializedDictionary |
	dictionary := Dictionary new.
	ephemeron := WeakKeyAssociation new
		key: Object new;
		value: 'value';
		container: dictionary.
	dictionary add: ephemeron.

	self deny: dictionary isEmpty.
	
	self serialize: dictionary.
	Smalltalk garbageCollect.
	self assert: dictionary isEmpty.
	
	materializedDictionary := self materialized.
	self deny: materializedDictionary isEmpty.
	self assert: materializedDictionary size equals: 1.
	self assert: materializedDictionary associations first key isNil.
	self assert: materializedDictionary associations first value equals: 'value'.
	
	Smalltalk garbageCollect.
	self deny: materializedDictionary isEmpty
]

{ #category : 'tests' }
FLEphemeronTest >> testWeakKeyAssociationMourningBeforeSerialization [
	"This tests serialization / materialization of WeakKeyAssociation, which is an ephemeron class.
	When the key is not strongly referenced, which is the case here, the garbage collector
	will send #mourn to the object to finalize it. This will cause the ephemeron to remove itself
	from its container."

	| dictionary ephemeron |
	dictionary := Dictionary new.
	ephemeron := WeakKeyAssociation new
		key: Object new;
		value: 'value';
		container: dictionary.
	dictionary add: ephemeron.

	Smalltalk garbageCollect.
	self assert: dictionary isEmpty
]

{ #category : 'tests' }
FLEphemeronTest >> testWeakKeyAssociationWithStrongReference [
	"This tests serialization / materialization of WeakKeyAssociation, which is an ephemeron class.
	When the key is strongly referenced, which is the case here, the garbage collector will
	not finalize the ephemeron."

	| dictionary key ephemeron materializedDictionary |
	dictionary := Dictionary new.
	key := Object new.
	ephemeron := WeakKeyAssociation new
		key: key;
		value: 'value';
		container: dictionary.
	dictionary add: ephemeron.

	self deny: dictionary isEmpty.
	
	self serialize: dictionary.
	Smalltalk garbageCollect.
	self deny: dictionary isEmpty.
	
	materializedDictionary := self materialized.
	self deny: materializedDictionary isEmpty.
	self assert: materializedDictionary size equals: 1.
	self assert: materializedDictionary associations first key isNil.
	self assert: materializedDictionary associations first value equals: 'value'.
	
	Smalltalk garbageCollect.
	self deny: materializedDictionary isEmpty
]

{ #category : 'tests' }
FLEphemeronTest >> testWeakKeyAssociationWithStrongReferenceInGraph [
	"This tests serialization / materialization of WeakKeyAssociation, which is an ephemeron class.
	When the key is strongly referenced, which is the case here, the garbage collector will
	not finalize the ephemeron.
	If the ephemeron's key is strongly referenced from inside the graph the key should also
	be serialized."

	| dictionary ephemeron materializedDictionary |
	dictionary := Dictionary new.
	ephemeron := WeakKeyAssociation new
		key: Object new;
		value: 'value';
		container: dictionary.
	dictionary add: ephemeron.

	self deny: dictionary isEmpty.
	
	self serialize: {dictionary. ephemeron key}.
	Smalltalk garbageCollect.
	self deny: dictionary isEmpty.
	
	materializedDictionary := self materialized first.
	self deny: materializedDictionary isEmpty.
	self assert: materializedDictionary size equals: 1.
	self assert: materializedDictionary associations first key class identicalTo: Object.
	self assert: materializedDictionary associations first value equals: 'value'.
	
	Smalltalk garbageCollect.
	"As there's no strong reference to the materialized key, the ephemeron should
	be finalized by the garbage collector and remove itself from its container."
	self assert: materializedDictionary isEmpty
]
