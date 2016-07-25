I am EndianDetector, a class responsible of detecting the endianess in the current machine.

I do so in my #calcEndianess method, which works as follows:

 - it creates a float object, whose endianess is managed automatically by the VM.
 - copy the raw data of that float object into a bytes object (instance of my class)
 - then, we can check in the raw copy if the data of the float is in the least or more significant word of the object.

Internally, I do the raw data copy by changing the class of the float into me. To do that I have the following properties:
- I'm a compact class (required to change the class)
- I'm in the startup list to check this on every startup