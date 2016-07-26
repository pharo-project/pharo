private
scheduleFetchFor: packageSpec externalReference: reference
  ^ self
    scheduleFetchFor: packageSpec
    reference: reference
    message:
      'Fetched -> ' , reference name , ' --- '
        , reference repository repositoryDescription , ' --- '
        , reference repository description