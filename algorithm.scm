
(import (prefix (chez batched-queue) q:))

(define dfs
  (lambda (v G)
    (if (not (has-vertex? G v))
	empty-graph
	(let ((seen (make-hash-table))
	      (tree (vertex v)))
	  (hashtable-set! seen v #t)
	  (let loop ((x v))
	    (for-each (lambda (y)
			(unless (hashtable-ref seen y #f)
			  (hashtable-set! seen y #t)
			  (set! tree (overlay (edge x y) tree))
			  (loop y)))
		      (adjacent x G)))
	  tree))))

(define bfs
  (lambda (v G)
    (if (not (has-vertex? G v))
	empty-graph
	(let ((seen (make-hash-table))
	      (tree (vertex v)))
	  (hashtable-set! seen v #t)
	  (let loop ((Q (q:snocq q:empty v)))
	    (unless (q:empty? Q)
	      (let ((x (q:headq Q)))
		(hashtable-set! seen x #t)
		(loop (fold-left (lambda (Q y)
				   (cond ((hashtable-ref seen y #f) Q)
					 (else
					  (hashtable-set! seen y #t)
					  (set! tree (overlay (edge x y) tree))
					  (q:snocq Q y))))
				 (q:tailq Q)
				 (adjacent x G))))))
	  tree))))


