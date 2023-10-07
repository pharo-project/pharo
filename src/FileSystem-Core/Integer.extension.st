Extension { #name : 'Integer' }

{ #category : '*FileSystem-Core' }
Integer >> humanReadableByteSizeString [
	"Return the receiver as a string with the adequate file size identified, e.g. '50 KB'. The
	difference with humanReadableByteSISizeString is that the current one counts by 1024 and
	not 1000."

	"(1000 * 1000 * 1000) humanReadableByteSizeString >>> '953.67 MB'"
	"(1000 * 1000 * 1000) humanReadableSISizeString >>> '1.00 GB'"
	"(1024 * 1024 * 1024) humanReadableByteSizeString >>> '1.00 GB'"

	^ self humanReadableByteSizeStringPrecision: 2
]

{ #category : '*FileSystem-Core' }
Integer >> humanReadableByteSizeStringPrecision: n [

	#( 'B' 'KB' 'MB' 'GB' 'TB' 'PB' 'EB' 'ZB' 'YB' )
		inject: self
		into: [ :value :each |
			value < 1024
				ifFalse: [ value / 1024 ]
				ifTrue: [ ^ (value printShowingDecimalPlaces: n) , ' ' , each ] ]
]

{ #category : '*FileSystem-Core' }
Integer >> humanReadableSIByteSize [
	"Return the receiver as a string with SI binary (International System of Units) file size, e.g. '50 KB'"

	self
		deprecated: 'use humanReadableSISizeString'
		transformWith:  '`@rec humanReadableSIByteSize' ->  '`@rec humanReadableSISizeString'.

	^ self humanReadableSISizeString
]
