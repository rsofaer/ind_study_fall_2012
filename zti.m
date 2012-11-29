function B=zti(A)
  B = arrayfun(@ztie, A);
end
function b=ztie(a)
 if(a == 0)
   b = inf;
 else
   b = a;
 end
end
