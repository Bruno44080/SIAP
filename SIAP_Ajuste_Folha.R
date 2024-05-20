folha_servidor <- function(
    pasta = 'C:\\SIAP',
    arquivo = 'FolhaServidor.csv',
    ano = '2024',
    competencia = '04'
){
  folha <- read.csv(file = paste0(pasta,'\\',ano,'\\',competencia,'\\Brutos\\',arquivo),header = F,sep = '|',
                    encoding = 'latin1')
  
  # V1: Código da Entidade de Origem
  #folha$V1
  #nchar(folha$V1)
  # V2: CPF da Pessoa Paga
  #folha$V2
  completar <- 11-nchar(folha$V2)
  for(i in 1:length(folha$V2)) folha$V2[i] <- paste0(c(rep(0,completar[i]),folha$V2[i]),collapse = '')
  folha$V3 # V3: Número da Matrícula da Pessoa Paga
  folha$V4 # V4: Identificação do Tipo de Situação do Servidor *
  folha$V5 # V5: Identificação do Tipo de Servidor Ativo
  folha$V6 # V6: Código de Controle da Função utilizada pela Entidade *
  folha$V7 # V7: Nome da Função utilizada pela Entidade
  folha$V8 # V8: Nome da Lotação
  # V9: Percentual de Desconto Patronal
  folha$V9 <- sprintf(folha$V9, fmt = '%#05.2f')
  # V10: Percentual de Desconto do Servidor 9.99
  folha$V10 <- sprintf(folha$V10, fmt = '%#05.2f')
  folha$V11 # V11: Identificação do Vínculo Previdenciário
  folha$V12 # V12: Código da Entidade do Servidor (campo somente para o layout da entidade Estado do Paraná)
  write.table(x = folha,file = paste0(pasta,'\\',ano,'\\',competencia,'\\',arquivo),quote = F,sep = '|',row.names = F,
              col.names = F,na = '',dec ='.',fileEncoding = 'latin1')
  
}
folha_verbas <- function(
    pasta = 'C:\\SIAP',
    arquivo = 'FolhaVerbas.csv',
    ano = '2024',
    competencia = '04'){ 
  folha <- read.csv(file = paste0(pasta,'\\',ano,'\\',competencia,'\\Brutos\\',arquivo),
                    header = F,sep = '|',encoding = 'latin1')
  # folha$V1 # V1: Código da Entidade de Origem
  # folha$V2 # V2: CPF da Pessoa Paga
  completar <- 11-nchar(folha$V2)
  for(i in 1:length(folha$V2)) folha$V2[i] <- paste0(c(rep(0,completar[i]),folha$V2[i]),collapse = '')
  # folha$V3 # V3: Número da Matrícula da Pessoa Paga
  # folha$V4 # V4: Identificação do Tipo de Situação do Servidor
  # folha$V5 # V5: Código do controle da Verba do Servidor
  # folha$V6 # V6: Nome da Verba do Servidor
  folha$V6[folha$V6 == 'ADICIONAL TEMPO SERVIÇO'] <- 'ADICIONAL TEMPO SERVIÇO - DE 5% A 45%'
  folha$V6[folha$V6 == 'GRAT.RESPONS.TÉCNICA - DE 50% A 100% DO NÍVEL INICIAL'] <- 'GRAT.RESPONS.TECNICA - DE 50% A 100% DO NIVEL INICIAL'
  folha$V6[folha$V6 == 'A.T.S. FERIAS'] <- 'A.T.S. FÉRIAS - 5% a 45%'
  folha$V6[folha$V6 == 'PREVID COMPLEM COMPULSORI'] <- 'PREVID COMPLEM COMPULSORI - de 3% a 100%'
  folha$V6[folha$V6 == 'AD TPO SERVICO 13O INTEGR'] <- 'AD TPO SERVIÇO 13º INTEGR - DE 5% A 45%'
  folha$V6[folha$V6 == 'ADIC TPO SERVICO 13O ADTO'] <- 'ADIC TPOSERVIÇO 13º ADTO DE 5% A 45%'
  folha$V6[folha$V6 == 'SUBSIDIOS'] <- 'SUBSÍDIOS'
  folha$V6[folha$V6 == 'SUBSIDIOS FÉRIAS'] <- 'SUBSÍDIOS FÉRIAS'
  folha$V6[folha$V6 == 'MEDIA H.EXTRAS 13O ADTO'] <- 'MÉDIA H. EXTRAS 13º ADTO'
  folha$V6[folha$V6 == 'SUBSIDIOS 13O ADTO'] <- 'SUBSÍDIOS 13º ADTO'
  folha$V6[folha$V6 == 'SUBSIDIOS 130 INTEGRAL'] <- 'SUBSÍDIOS 13º INTEGRAL'
  folha$V6[folha$V6 == '13O SAL. MATERNIDADE EST'] <- '13º SAL. MATERNIDADE EST'
  folha$V6[folha$V6 == 'GRAT.RESP.TECNICA MATERN'] <- 'GRAT.RESP.TECNICA MATERN - DE 50% A 100%'
  folha$V6[folha$V6 == 'HORAS SAL.MATERNIDADE EST'] <- 'HORAS SAL. MATERNIDADE EST'
  folha$V6[folha$V6 == 'A.T.S. MATERNIDADE EST'] <- 'A.T.S. MATERNIDADE EST 5% a 45%'
  
  #folha$V7 # V7: Valor da Verba
  folha$V7 <- sprintf(folha$V7, fmt = '%#0.2f')
  # folha$V8 # V8: Operador da Verba
  # folha$V9 # V9: Número do Empenho
  # folha$V10 # V10: Ano do Empenho
  # folha$V11 # V11: Código da Entidade de Origem do Empenho
  write.table(x = folha,file = paste0(pasta,'\\',ano,'\\',competencia,'\\',arquivo),quote = F,dec = '.',sep = '|',row.names = F,
              col.names = F,na = '',fileEncoding = 'latin1')
}
