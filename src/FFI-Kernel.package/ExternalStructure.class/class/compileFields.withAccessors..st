field definition
compileFields: specArray withAccessors: aBool 
	"Define all the fields in the receiver. 
	Return the newly compiled spec."
	| byteOffset typeSpec |
	(specArray size > 0 and: [specArray first class ~~ Array]) ifTrue:
		[^ self compileAlias: specArray withAccessors: aBool].
	byteOffset := 1.
	typeSpec := WriteStream on: (WordArray new: 10).
	typeSpec nextPut: FFIFlagStructure.
	"dummy for size"
	specArray do:
		[:spec | | fieldName fieldType isPointerField externalType typeSize selfRefering |
		fieldName := spec first.
		fieldType := spec second.
		isPointerField := fieldType last = $*.
		fieldType := (fieldType findTokens: ' *') first.
		externalType := ExternalType atomicTypeNamed: fieldType.
		selfRefering := isPointerField and: [externalType isNil and: [fieldType = self asString]].
		selfRefering ifTrue: [
			externalType := ExternalType void asPointerType
		] ifFalse:[
			externalType == nil ifTrue: ["non-atomic"
				Symbol
					hasInterned: fieldType
					ifTrue: [:sym | externalType := ExternalType structTypeNamed: sym].
			].
			externalType == nil ifTrue:[
				Transcript show: '(' , fieldType , ' is void)'.
				externalType := ExternalType void.
			].
			isPointerField ifTrue: [externalType := externalType asPointerType: self pointerSize]].
			typeSize := externalType byteSize.
			spec size > 2 ifTrue: ["extra size"
				spec third < typeSize
					ifTrue: [^ self error: 'Explicit type size is less than expected'].
				typeSize := spec third.
			].
			(fieldName notNil and: [aBool]) ifTrue: [
				self defineFieldAccessorsFor: fieldName startingAt: byteOffset type: externalType.
			].
			typeSpec nextPutAll: (externalType embeddedSpecWithSize: typeSize).
			byteOffset := byteOffset + typeSize.
		].
	compiledSpec := typeSpec contents.
	compiledSpec at: 1 put: (byteOffset - 1 bitOr: FFIFlagStructure).
	ExternalType noticeModificationOf: self.
	^ compiledSpec