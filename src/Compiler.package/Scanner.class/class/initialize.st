class initialization
initialize
	"The unicode ending with FFFE or FFFF are non characters and can be used by applications if they wish.
	We use last legal unicode 16r10FFFF to encode the end of source stream"
	DoItCharacter := Character value: 16r10FFFF