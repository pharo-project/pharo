Extension { #name : #Stdio }

{ #category : #'*FileSystem-Memory' }
Stdio class >> useMemoryStreams [
	<script>
	self createWriteStreamBlock: [ :aName | (FileSystem memory / aName asString) writeStream ]
]
