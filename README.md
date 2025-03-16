# Clasificator - Tema LFA
## Horia Mercan - 333CA

Pentru rezolvarea taskului propus, voi folosi o structura pe trie pentru a obtine
prefixele comune ale cuvintelor. Astfel, pentru exemplul de input:
```
3 2 4
baed
bece
bace
bacd
aace
```

vom organiza urmatorul trie:
```
Nothing
        -Just 'a'
                -Just 'a'
                        -Just 'c'
                                -Just 'e' .WSFail
        -Just 'b'
                -Just 'a'
                        -Just 'c'
                                -Just 'd' .WSFail
                                -Just 'e' .WSAccept
                        -Just 'e'
                                -Just 'd' .WSAccept
                -Just 'e'
                        -Just 'c'
                                -Just 'e' .WSAccept
```

fiecare frunza stiind daca respectivul cuvant face parte din multimea ce trebuie acceptata, respectiv respinsa de regex-ul creat. Vom face urmatoarea remarca:
- Daca pentru un anumit nod, toate frunzele copil ale acestuia sunt Accepted, atunci o expresie regulata care match-uieste aceste cuvinte este
(<b>\<prefixul acestor cuvinte>.*</b>). Folosind aceasta optimizare, vom construi o expresie regulata care sa suprapuna toate prefixele cat mai mult posibil.