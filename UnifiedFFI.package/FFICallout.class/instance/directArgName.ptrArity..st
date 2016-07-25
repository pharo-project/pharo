private
directArgName: typeName ptrArity: ptrArity
	"Answer 'direct' type arguments (without argument names): 
		self, nil,true, false or a constant (like a number)"

	typeName = 'nil'
		ifTrue: [ ^ FFIConst value: 0 type: (self resolveExternalType: #uint32) ].
	typeName = 'false'
		ifTrue: [ ^ FFIConst value: 0 type: (self resolveExternalType: #bool) ].
	typeName = 'true'
		ifTrue: [ ^ FFIConst value: 1 type: (self resolveExternalType: #bool) ].
	ptrArity > 0
		ifTrue: [ self error: 'missing argument name' ].	"lone self"
	typeName = 'self'
		ifTrue: [ 
			^ (requestor asExternalTypeOn: self) 
				prepareAsSelfFromCalloutDeclaration;
				loader: self receiverArgumentLoader ].
	^ self resolveType: typeName