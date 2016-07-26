testing - associaitons
includesAssociation: anAssociation
  ^ (self   
      associationAt: anAssociation key
      ifAbsent: [ ^ false ]) value = anAssociation value
