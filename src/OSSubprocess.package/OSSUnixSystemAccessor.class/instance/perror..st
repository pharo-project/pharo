errors
perror: aMessage
	"Prints the errno gloabl variable into the stderr together with aMessage string
	and then signal an error"
	self primitivePerror: aMessage.
	self error: 'Error: ', aMessage,' You may want to check errors in stderr'