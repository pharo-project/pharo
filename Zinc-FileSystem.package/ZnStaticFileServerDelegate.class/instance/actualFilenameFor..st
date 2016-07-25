private
actualFilenameFor: uri
	| subElements subDir entry |
	(uri isEmpty and: [ self prefix isEmpty ]) ifTrue: [ ^ self indexFileIn: self directory ].
	(self prefix isEmpty or: [ uri isEmpty not and: [ uri pathSegments beginsWith: self prefix ] ]) ifFalse: [ ^ nil ].
	subElements := (uri pathSegments allButFirst: self prefix size) reject: [ :each | each = #/ ].
	subDir := subElements allButLast inject: self directory into: [ :parent :sub | | file |
		(file := parent / sub) exists
			ifTrue: [ file ]
			ifFalse: [ ^ nil ] ]. 
	subElements isEmpty
		ifTrue: [ entry := subDir entry ] 
		ifFalse: [ | file |
			 (file := subDir / subElements last) exists 
				ifTrue: [ entry := file entry ] 
				ifFalse: [ ^ nil ] ].
	^ entry isDirectory 
		ifTrue: [ self indexFileIn: entry reference ]
		ifFalse: [ entry reference fullName ]