I am ZnUrlOperation.
I am an abtract superclass of operations that can be performed on ZnUrl objects.

Some operations, like retrieving the contents of a resource described by a URL, depend on elements, most likely the scheme, of the URL. By creating the appropriate subclasses, this behavior can be added.

I implement searching/matching logic on my class side.

  #performOperation:with:on:

which defaults to matching the #operation and #schemes.

I am instanciated to be executed. 

  #performOperation

A concrete example is ZnUrl>>#retrieveContents with subclasses ZnHttpRetrieveContents and ZnFileRetrieveContents.

Part of Zinc HTTP Components.