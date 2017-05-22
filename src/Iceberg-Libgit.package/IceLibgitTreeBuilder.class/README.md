Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, "I represent a paragraph of text".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	builder:		<Object>
	changes:		<Object>
	children:		<Object>
	entryName:		<Object>
	originalTree:		<LGitTree> Contains the original contents on which this builder bases. Warning: this will not reflect the actual contents of the builder. Use #buildTree instead.
	parent:		<Object>


    Implementation Points