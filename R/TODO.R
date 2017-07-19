# TODO LIST

# make sure pbapply package gets loaded in metabarcodedb

# check the function check_and_get_bold_specimens once have response to potential bug in bold::bold_specimens
# (raised at https://github.com/ropensci/bold/issues/46)

# Improve message output by map_uids_to_bold_and_accn so that it accurately reports the record #s when
# dealing with > record_block_size records

# Improve the behaviour of chunk-skipping in check_and_do_classification so that you can actually
# stop the command instead of having it move on to the next chunk repeatedly

# Improve behaviour of check_and_get_seqs when fetching by id vector: get it to report progress better.

# Improve behaviour of check_and_get_seqs: currently writes to file still if unable to retrieve seqs. Need to fix this...

# convert BOLD output to upper case

# double-check whether need to fix name line so that it ends with a ;