Extension { #name : #Unicode }

{ #category : #'*Deprecated10' }
Unicode class >> isSimplifiedChinese: code [

	self deprecated: 'Will be removed' on: ' 23 July 2021' in: #Pharo10.
	^ code > 255 and: [(GB2312 charFromUnicode: code) notNil]



]
