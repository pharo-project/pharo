accessing
keysDo: block
 
 	self do: [:item | block value: (keyBlock value: item)]