I represent a directory. I provide the minimum operations to:

- create a directory
- delete the directory
- access its children

!Examples of usage

"Creating a directory"
dir := Directory named: 'test-dir'.

"Accessing its properties"
dir name.
dir exists.

"Opening / closing it"
dir create.
dir delete.

"Accessing its children"
dir children.
dir childNamed: 'child'.