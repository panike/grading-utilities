" This is for downloading the class roster from MyUW.

function! Massage_data()
	:s/.\+\t.\+\t\(.\+\)\t\(.\+\)/(insert-name "\1" *the-arr* "\2")/
endfunction
map <F5> :call Massage_data()<Return>j

" Deleting a student. You should search, then hit the f6 function key
map <F6> ddn

" Finish with the file
map <F7> :wn<Return>
