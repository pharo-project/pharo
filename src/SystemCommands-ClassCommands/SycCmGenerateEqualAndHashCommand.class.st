"
Command to execute a Transformation for generating #hash and #= comparing methods.
"
Class {
	#name : 'SycCmGenerateEqualAndHashCommand',
	#superclass : 'SycCmGenerateCommand',
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'private - contents' }
SycCmGenerateEqualAndHashCommand class >> equalIconContents [
	" Equal icons created by Angelo Troiano - Flaticon "
	^ 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAJZlWElmTU0AKgAAAAgABQESAAMAAAABAAEAAAEaAAUAAAABAAAASgEbAAUAAAABAAAAUgExAAIAAAARAAAAWodpAAQAAAABAAAAbAAAAAAAAABgAAAAAQAAAGAAAAABd3d3Lmlua3NjYXBlLm9yZwAAAAOgAQADAAAAAQABAACgAgAEAAAAAQAAABCgAwAEAAAAAQAAABAAAAAAKwl1NwAAAAlwSFlzAAAOxAAADsQBlSsOGwAAAi1pVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDYuMC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyI+CiAgICAgICAgIDx4bXA6Q3JlYXRvclRvb2w+d3d3Lmlua3NjYXBlLm9yZzwveG1wOkNyZWF0b3JUb29sPgogICAgICAgICA8dGlmZjpZUmVzb2x1dGlvbj45NjwvdGlmZjpZUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6T3JpZW50YXRpb24+MTwvdGlmZjpPcmllbnRhdGlvbj4KICAgICAgICAgPHRpZmY6WFJlc29sdXRpb24+OTY8L3RpZmY6WFJlc29sdXRpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgryNKKqAAAAsklEQVQ4EcWRSwpCMQxF6wdEXJFzZ4LoRlyKW1EEZ85djwMRB37OAS2B54OAioEDbZqb3qal/Ds6wUCPddyHo8byTuYas1lhQ6OwCzdYwBTOYO5dWDeEHayh1mlfodYyWKum9MEuvmcJM8g42D411QH7z+IrQxzjYQIXaGvofAawhwPUJziQI2QGaI21dYje5hBXMIcT1M6sY/iNI9iAmjanHCUjdtCSN2Ti9fWZ2h/XPABvpjDTxqqLbwAAAABJRU5ErkJggg=='
]

{ #category : 'executing' }
SycCmGenerateEqualAndHashCommand >> driverClass [

	^ ReGenerateEqualAndHashDriver
]

{ #category : 'accessing' }
SycCmGenerateEqualAndHashCommand >> icon [

	^ Form fromBinaryStream: self class equalIconContents base64Decoded asByteArray readStream
]

{ #category : 'testing' }
SycCmGenerateEqualAndHashCommand >> isApplicable [
	"Answer <true> if the receiver has any variables"

	^ context lastSelectedClass realClass slots notEmpty
]

{ #category : 'accessing' }
SycCmGenerateEqualAndHashCommand >> order [
	^ 41
]
