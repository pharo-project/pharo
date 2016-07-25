I am the superclass of all refactoring change objects. 
All I have is a name for the refactoring, but I can perform one or more refactoring operations with the message #execute. 
I am a composite object. To know about my components, ask me with #changes and #changesSize. 