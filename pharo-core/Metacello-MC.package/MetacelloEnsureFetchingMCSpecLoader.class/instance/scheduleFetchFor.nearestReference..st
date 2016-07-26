private
scheduleFetchFor: packageSpec nearestReference: reference
  "latest version in repository already matches the cached reference...schedule a load directive for reference, so ensured load will come from cache"

  ^ self
    scheduleFetchFor: packageSpec
    reference: reference
    message:
      'Fetched -> (nearest) ' , reference name , ' --- '
        , reference repository repositoryDescription , ' --- '
        , reference repository description