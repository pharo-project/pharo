accessing
name
	^ self spec 
		ifNil: [ 'nil' ]
		ifNotNil: [ :specif | specif name ]