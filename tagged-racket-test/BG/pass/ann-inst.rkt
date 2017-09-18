#lang tagged/racket/base

((ann car (All (A B) (-> (Pairof A B) A))) '(A B))
((inst car Symbol (Listof Symbol)) '(A B))
