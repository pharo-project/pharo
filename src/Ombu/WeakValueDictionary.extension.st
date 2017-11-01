Extension { #name : #WeakValueDictionary }

{ #category : #'*Ombu' }
WeakValueDictionary >> at: key ifPresent: presentBlock ifAbsentOrNil: absentBlock [

	^ self 
		at: key
		ifPresent: [:valueOrNil | 
			valueOrNil
				ifNotNil: presentBlock
				ifNil: absentBlock ]
		ifAbsent: absentBlock
]
