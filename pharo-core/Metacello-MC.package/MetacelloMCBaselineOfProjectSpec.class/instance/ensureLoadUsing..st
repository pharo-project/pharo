loading
ensureLoadUsing: aLoader
  "see https://github.com/dalehenrich/metacello-work/issues/244 ... uncoditionally load
   baseline"

  [ super ensureLoadUsing: aLoader ]
    on: MetacelloIgnorePackageLoaded
    do: [ :ex | ex resume: ex packageSpec name = self name ]