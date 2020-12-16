Class {
	#name : #TFStructTest,
	#superclass : #TFTestCase,
	#category : #'ThreadedFFI-Tests'
}

{ #category : #tests }
TFStructTest >> longStructSize [ 

	^ TFWorker default
		invokeFunction:
			(TFExternalFunction
				name: 'sizeOfLongStruct'
				moduleName: self libraryPath
				definition:
					(TFFunctionDefinition parameterTypes: {} returnType: TFBasicType uint))
		withArguments: #()

]

{ #category : #tests }
TFStructTest >> pointSize [

	^ TFWorker default
		invokeFunction:
			(TFExternalFunction
				name: 'sizeOfPoint'
				moduleName: self libraryPath
				definition:
					(TFFunctionDefinition parameterTypes: {} returnType: TFBasicType uint))
		withArguments: #()
]

{ #category : #tests }
TFStructTest >> testLongStructSize [
	| pointType |
	pointType := TFStructType
		forClass: TFLongTestStruct
		withMembers:
			(Array new: 100 withAll: TFBasicType schar)
				,	{TFBasicType float.
					TFBasicType double.
					TFBasicType slong.
					TFBasicType sint.
					TFBasicType sshort}.
	self assert: pointType byteSize equals: self longStructSize.
	self assert: TFLongTestStruct byteSize equals: self longStructSize
]

{ #category : #tests }
TFStructTest >> testPointStructSize [

	| pointType |
	pointType := TFStructType forClass: TFPointTestStruct withMembers: { TFBasicType sint. TFBasicType sint. }.
	
	self assert: pointType byteSize equals: self pointSize.
	self assert: pointType byteSize equals: TFPointTestStruct byteSize.
	
]

{ #category : #tests }
TFStructTest >> testReturnsAnStruct [
	| pointType fun aPoint |
	
	pointType := TFStructType 
		forClass: TFPointTestStruct 
		withMembers: { 
			TFBasicType sint. 
			TFBasicType sint }.
	
	fun := TFExternalFunction
		name: 'newPoint' moduleName: self libraryPath
		parameterTypes:	{TFBasicType sint. TFBasicType sint}
		returnType: pointType.
		
	aPoint := TFPointTestStruct fromHandle: (runner invokeFunction: fun withArguments: #(1 5)).

	self assert: aPoint x equals: 1.
	self assert: aPoint y equals: 5.	
]
