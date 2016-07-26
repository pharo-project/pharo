converting
asSeconds
 	"Return the number of seconds since the Squeak epoch"
 
 	^ (self - (self class epoch)) asSeconds