# ReadMe

ReadMe for v2.1 of the Dynamically Generated Hate Speech Dataset, described in 'Learning from the Worst'.

'acl.id' is the unique ID of the entry.

'Text' is the content which has been entered. All content is synthetic.

'Label' is a binary variable, indicating whether the content has been identified as hateful. It takes two values: hate, nothate.

'Type' is a categorical variable, providing a secondary label for hateful content. For hate it can take five values: Animosity, Derogation, Dehumanization, Threatening and Support for Hateful Entities. Please see the paper for more detail. For nothate the 'type' is 'none'. Note that in round 1 the 'type' was not given and is marked as 'notgiven'.

'Target' is a categorical variable, providing the group that is attacked by the hate. It can include intersectional characteristics and multiple groups can be attacked. For nothate the type is 'none'. Note that in round 1 the 'target' was not given and is marked as 'notgiven'.

'Level' reports whether the entry is original content or a perturbation.

'Round' is a categorical variable. It gives the round of data entry (1, 2, 3 or 4) with a letter for whether the entry is original content ('a') or a perturbation ('b'). Perturbations were not made for round 1.

'Round.base' is a categorical variable. It gives the round of data entry, indicated with just a number (1, 2, 3 or 4).

'Split' is a categorical variable. it gives the data split that the entry has been assigned to. This can take the values 'train', 'dev' and 'test'. The choices of splits is explained in the paper.

'Annotator' is a categorical variable. It gives the annotator who entered the content. Annotator IDs are random alphanumeric strings. There are 20 annotators in the dataset.

'acl.id.matched' is the ID of the matched entry, connecting the original (given in 'acl.id') and the perturbed version.

