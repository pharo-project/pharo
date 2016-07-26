I clusterize objects that will be obtained on materialization via a message send to a global object. 

Explained with an example:

Suppose we have a special instance of User that represents the admin user, and it is a unique instance in the image. In case the admin user is referenced in our graph, we want to treat that object as a global. We can do that in this way:

User >> fuelAccept: aVisitor
    ^self == User admin
        ifTrue: [aVisitor visitGlobalSend: self]
        ifFalse: [super fuelAccept: aVisitor]

User >> fuelGlobalName
    ^#User

User >> fuelSelector
    ^#admin

So what will happen is that during serialization, the admin user won't be completly serialized (with all its intance variables) but instead its global name and selector are stored. Then, at materialization time, Fuel will send the selector #admin to the class User, and use what that answers as the admin user of the materialized graph.

We test this feature in FLGlobalSendSerializationTest.