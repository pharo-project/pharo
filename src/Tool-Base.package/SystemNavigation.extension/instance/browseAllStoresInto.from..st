*Tool-Base
browseAllStoresInto: instVarName from: aClass
    "Create and schedule a Message Set browser for all the receiver's methods 
    or any methods of a subclass/superclass that refer to the instance variable name."

    "self new browseAllStoresInto: 'youpi' from: BalloonEdgeData class."

    ^ self browseMessageList: (self allStoresInto: instVarName of: aClass)
                name: 'Stores into ' , instVarName 
                autoSelect: instVarName
