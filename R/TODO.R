# TODO LIST

# There is a mistake in check_and_get_seqs: doesn't detect properly when records are present in the mapping file
# but absent from the fasta file, so doesn't appear to update properly. Not sure why? Very important to
# troubleshoot this. I think it might be a problem that happens if you use different block_size in the different commands?
### FIXED ###

# There is a major problem with check_and_get_seqs: not sure exactly what's happening, but it looks like
# the first list element just gets repeated when using get_fasta_with_web_history...?
# Looks like this problem comes from get_ncbi_search_blocks...
### FIXED ###

# Improve message output by check_and_get_seqs so it accurately reports the progress through the total
# sequences it's obtaining
### FIXED ###

# check_and_get_seqs is obtaining some sequences more than once? (think this is also to do with block size?)
# have observed this when getting sequences from scratch (i.e. when using get_fasta_with_web_history)
### think this is fixed? ###

# Improve message output by map_uids_to_bold_and_accn so that it accurately reports the record #s when
# dealing with > record_block_size records

# Improve message output by get_ncbi_search_blocks so that it accurately reports the IDs being retrieved
# (currently says 'Getting NCBI IDs 5001 to 10001 of 7237', for example)
### FIXED ###

# Improve the behaviour of chunk-skipping in check_and_do_classification so that you can actually
# stop the command instead of having it move on to the next chunk repeatedly

# Improve behaviour of check_and_get_seqs when fetching by id vector: get it to report progress better.

# Improve behaviour of check_and_get_seqs: currently writes to file still if unable to retrieve seqs. Need to fix this...

# occasionally NCBI FASTA records have > characters within the sequence name! This stuffs up
# readability by ape::read.FASTA. Currently fixing this manually...
### have now fixed this in the make_dada2_ref_database command

# convert output to upper case

# fix name line so that it ends with a ;