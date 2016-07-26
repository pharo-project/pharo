write and read
writeInstance: anObject
    "PRIVATE -- Write the contents of an arbitrary instance."

    ^ anObject storeDataOn: self