compiling
compile: code notifying: requestor 
	"Refer to the comment in Behavior|compile:notifying:." 

	^self compile: code
		 classified: Protocol unclassified
		 notifying: requestor