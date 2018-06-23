Extension { #name : #Stdio }

{ #category : #'*FileSystem-Memory' }
Stdio class >> useMemoryStreams [
	<script>
	self
		createWriteStreamBlock: [ :aName | (FileSystem newMemory / aName asString) writeStream ]
]
