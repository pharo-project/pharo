I am NotFound, an exception to indicate that something is not found in a collection.
I am an Error and thus Exception.

Typically, the thing not found is in my object instance variable.
The collection where this thing was not found is in my inherited signaler instance variable.

[ NotFound signalFor: 10 in: #(1 2 3) ] on: NotFound do: [ :exception | exception ]