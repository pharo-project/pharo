morphic ui
buttonSpecs
       ^ #((Browse browse 'Browse this version' hasVersion)
               (History history 'Browse the history of this version' hasVersion)
               (Changes changes 'Browse the changes this version would make to the
image' hasVersion)
               (Load load 'Load this version into the image' hasVersion)
               (Merge merge 'Merge this version into the image' hasVersion)
               (Adopt adopt 'Adopt this version as an ancestor of your working copy'
hasVersion)
               (Copy save 'Copy this version to another repository' hasVersion)
               (Diff diff 'Create an equivalent version based on an earlier release'
hasVersion))