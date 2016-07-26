private
ensureDirectory: aStringOrFileReference distinctFrom: aDefaultDirectory
		" Ensure aStringOrFileReference is either nil or a valid file-reference object distinct from the default one.
	  Do not create any directory on disk yet - the first access to any directory will handle that.
	  #asFileReference and #absolutePath may raise anError! "
	| fileReference |

	^ [ 
		fileReference := aStringOrFileReference asFileReference.
		^ fileReference absolutePath = aDefaultDirectory absolutePath
			ifTrue: [ nil ]
			ifFalse: [ fileReference ]
			
		" Background: The behavior of the default directory is being relative to the image. Therefore the default file-reference is relative to the image. If the image is moved/copied around the file-reference will stay relative to the image. We assume this is the expected behavior for the default directory. Therefore we prevent the user from setting that value as an absolute file-reference, since an absolute file-reference would behave differently when an image is moved/copied around. "	
		]
			on: Error
			do: [ nil ]