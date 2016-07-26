comparing
<= aDANode 
	^ (aDANode class respondsTo: #packageName) and: [ self packageName <= aDANode packageName ]