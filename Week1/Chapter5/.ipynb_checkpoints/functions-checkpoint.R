bmi.func = function(height, weight){
    bmi = weight /(height * height)
    return (bmi)
}

danh_gia_bmi.function = function(bmi){
    result= ""
    if(bmi <18.5){
        result = "Gay"
    } else if (bmi <25){
        result ="binh thuong"
    }else{
        result = "thua can"
    }
    return (result)
}

ptb1.func = function(a,b){
    result = ""
    if(a == 0 & b!=0){
        result = 'PT vo nghiem'
    }else if(a ==0 & b ==0){
        result = "PT vo so nghiem"
    } else{
        result = paste("Nghiem:", -b/a)
    }
    return (result)
}

ptb2.func = function(a,b,c){
    result = ""
    if(a == 0){
        result = ptb1.func(b,c)
    }else{
        delta = b*b - 4*a*c
        if(delta <0){
            result = "PT vo nghiem"
        }else if(delta == 0){
            nghiem = -b/(2*a)
            result = paste("X1 = x2 = x0 =", nghiem)
        }else{
            x1 = (-b + sqrt(delta))/(2*a)
            x2 = (-b - sqrt(delta))/(2*a)
            result = paste("x1 =", x1, "x2=", x2)
        }
        
    }
    return (result)
}

tinh_tien_dien.func = function(so_kw){
   muc1=1678
    muc2=1734
    muc3=2014
    muc4=2536
    muc5=2834
    muc6=2927
    bac50=50
    bac100=100
    tien_dien=0
    if(so_kw<=50){
        tien_dien = so_kw *muc1
    } else if(so_kw <=100) {
        tien_dien = bac50 *muc1 + (so_kw -bac50)*muc2
    } else if(so_kw <=200) {
        tien_dien = bac50 *muc1 + bac50 *muc2 + (so_kw -bac100)*muc3
    }else if(so_kw <=300) {
        tien_dien = bac50 *muc1 + bac50 *muc2 + bac100*muc3 + (so_kw -bac50 -bac50 -bac100)*muc4
    }else if(so_kw <=400) {
        tien_dien = bac50 *muc1 + bac50 *muc2 + bac100*muc3+ bac100*muc4 + (so_kw -bac50 -bac50 -bac100 -bac100)*muc5
    }else{
        tien_dien = bac50 *muc1 + bac50 *muc2 + bac100*muc3+ bac100*muc4 + bac100*muc5 + (so_kw -bac50 -bac50 -bac100 -bac100-bac100)*muc6
    }
    
    return(tien_dien)
}