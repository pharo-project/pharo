Class {
	#name : #FLCompiledMethodSerializationTest,
	#superclass : #FLSerializationTest,
	#instVars : [
		'theCompiledMethod',
		'theClass'
	],
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #running }
FLCompiledMethodSerializationTest >> setUp [
	super setUp.
	theClass := self newClass class
]

{ #category : #tests }
FLCompiledMethodSerializationTest >> testDoIt [
	| materialized |
	"Since Pharo 3.0 decompilation of compiled methods is no longer possible.
	That means we have to store the source too."
	self useDoIt.

	theCompiledMethod selector: #DoIt.
	self assert: theCompiledMethod isDoIt.
	self assert: theCompiledMethod isInstalled.
	self deny: theCompiledMethod trailer hasSource.
	self assert: theCompiledMethod trailer isEmpty.
	self deny: theCompiledMethod trailer hasSourcePointer.

	materialized := self resultOfSerializeAndMaterialize: theCompiledMethod.
	"not possible since it's a different instance"
	self deny: materialized isInstalled.
	self assert: materialized isDoIt.
	"we serialized the source"
	self assert: materialized trailer hasSource.
	self deny: materialized trailer isEmpty.
	self deny: materialized trailer hasSourcePointer.

	self assert: (materialized isEqualRegardlessTrailerTo: theCompiledMethod)
]

{ #category : #tests }
FLCompiledMethodSerializationTest >> testInstalled [
	| materialized |
	"Since Pharo 3.0 decompilation of compiled methods is no longer possible.
	That means we have to store the source too."
	self useInstalled.

	self assert: theCompiledMethod isInstalled.
	self deny: theCompiledMethod isDoIt.
	self deny: theCompiledMethod trailer hasSource.
	self deny: theCompiledMethod trailer isEmpty.
	self assert: theCompiledMethod trailer hasSourcePointer.

	"if installed but not different, the installed instance will be answered"
	materialized := self resultOfSerializeAndMaterialize: theCompiledMethod.
	self assert: materialized identicalTo: theCompiledMethod
]

{ #category : #tests }
FLCompiledMethodSerializationTest >> testInstalledModified [
	| copy materialized |
	"Since Pharo 3.0 decompilation of compiled methods is no longer possible.
	That means we have to store the source too."
	self useInstalled.

	self assert: theCompiledMethod isInstalled.
	self deny: theCompiledMethod isDoIt.
	self deny: theCompiledMethod trailer hasSource.
	self deny: theCompiledMethod trailer isEmpty.
	self assert: theCompiledMethod trailer hasSourcePointer.

	copy := theCompiledMethod copy.
	"different instance can not be installed at the same time."
	self deny: copy isInstalled.
	self deny: copy isDoIt.
	self deny: copy trailer hasSource.
	self deny: copy trailer isEmpty.
	self assert: copy trailer hasSourcePointer.

	"if installed but not different, the installed instance will be answered"
	materialized := self resultOfSerializeAndMaterialize: copy.
	self deny: materialized identicalTo: theCompiledMethod.
	self deny: materialized identicalTo: copy.
	self deny: materialized isInstalled.
	self deny: materialized isDoIt.
	"we serialized the source"
	self assert: materialized trailer hasSource.
	self deny: materialized trailer isEmpty.
	self deny: materialized trailer hasSourcePointer
]

{ #category : #tests }
FLCompiledMethodSerializationTest >> testNotInstalled [
	| materialized |
	"Since Pharo 3.0 decompilation of compiled methods is no longer possible.
	That means we have to store the source too."
	self useNotInstalled.
	
	self deny: theCompiledMethod isInstalled.
	self deny: theCompiledMethod isDoIt.
	self deny: theCompiledMethod trailer hasSource.
	self assert: theCompiledMethod trailer isEmpty.
	self deny: theCompiledMethod trailer hasSourcePointer.
	
	materialized := self resultOfSerializeAndMaterialize: theCompiledMethod.
	self deny: materialized isInstalled.
	self deny: materialized isDoIt.
	"we serialized the source"
	self assert: materialized trailer hasSource.
	self deny: materialized trailer isEmpty.
	self deny: materialized trailer hasSourcePointer.
	
	self assert: (materialized isEqualRegardlessTrailerTo: theCompiledMethod)
]

{ #category : #helpers }
FLCompiledMethodSerializationTest >> useDoIt [
	theClass 
		duringTestCompileSilently: 'm  ^ 42'
		storeSource: false.
	theCompiledMethod := theClass methodNamed: #m.
	theClass methodDict
		at: #DoIt
		put: theCompiledMethod
]

{ #category : #helpers }
FLCompiledMethodSerializationTest >> useInstalled [
	theClass 
		duringTestCompileSilently: 'm  ^ 42'
		storeSource: true.
	theCompiledMethod := theClass methodNamed: #m
]

{ #category : #helpers }
FLCompiledMethodSerializationTest >> useNotInstalled [
	theClass 
		duringTestCompileSilently: 'm  ^ 42'
		storeSource: false.
	theCompiledMethod := theClass methodNamed: #m.
	theClass methodDict removeKey: #m
]
