instance creation
keyBlock: oneArgBlock
 	"Create a new KeySet whose way to access an element's key is by executing oneArgBlock on the element"
 
 	^ self new keyBlock: oneArgBlock