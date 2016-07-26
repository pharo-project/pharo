accessing
contents
	"Since this is a non-repeatable entity, 
	#contents will #consumeContent, 
	this can thus only be done once"
	
	^ ZnUtils readUpToEnd: self stream limit: ZnConstants maximumEntitySize