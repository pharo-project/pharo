ProtoObject establishes minimal behavior required of any object in Pharo, even objects that should balk at normal object behavior. 

Generally these are proxy objects designed to read themselves in from the disk, or to perform some wrapper behavior, before responding to a message. 

ProtoObject has no instance variables, nor should any be added.