accessing
characterCode
	"answer the unicode value"
	
	^ character ifNil: [0] ifNotNil: [ character charCode ] 