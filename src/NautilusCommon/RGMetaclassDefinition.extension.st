Extension { #name : #RGMetaclassDefinition }

{ #category : #'*NautilusCommon' }
RGMetaclassDefinition >> inheritsFrom: aClass [

	^ self realClass inheritsFrom: aClass
]
